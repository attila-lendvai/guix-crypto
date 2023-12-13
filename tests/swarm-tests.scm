;;; Copyright © 2021 Attila Lendvai <attila@lendvai.name>
;;;
;;; This file is part of guix-crypto, a channel for Guix.
;;;
;;; guix-crypto is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guix-crypto is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guix-crypto.  If not, see <http://www.gnu.org/licenses/>.

;; Run with something like this:
;; $(guix system --no-graphic vm ~/workspace/guix/guix-crypto/tests/swarm-tests.scm) -m 2048
;; $(./pre-inst-env guix system --no-graphic vm --share=$HOME/workspace/guix/var-lib-of-guest-vm=/var/lib ~/workspace/guix/guix-crypto/tests/swarm-tests.scm) -m 2048

(define-module (swarm-tests)
  #:use-module (shepherd-package) ; from a checkout of the shepherd git repo
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto service-utils)
  #:use-module (guix-crypto packages ethereum)
  #:use-module (guix-crypto packages swarm)
  #:use-module (guix-crypto services swarm)
  #:use-module (guix-crypto services lighthouse)
  #:use-module (guix-crypto services nethermind)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system nss)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services networking)
  #:use-module (gnu services sysctl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tmux)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (run-basic-test
            *test-swarm-starting*))

(define *use-custom-shepherd* #true)
;;(define *parent-shepherd-package* shepherd-0.10) ; from guix
(define *parent-shepherd-package* (@ (shepherd-package) shepherd)) ; from shepherd
(define *custom-shepherd-dir* "/home/alendvai/workspace/guix/shepherd")
(define *custom-shepherd-only-from-commits* #true)

;; (define-public custom-guile-fibers
;;   (package
;;     (inherit guile-fibers-next)
;;     (version "dev")
;;     (arguments
;;      `(;;#:tests? #false
;;        ,@(package-arguments guile-fibers-next)))
;;     (source (let ((dir "/home/alendvai/workspace/guix/fibers"))
;;               (if #true
;;                   (git-checkout
;;                    (url (string-append "file://" dir))
;;                    (branch "bit-count")
;;                    ;;(commit "a281ebfd4466a6925e264c303201b8dc44d33cb4")
;;                    )
;;                   (local-file dir
;;                               #:recursive? #t
;;                               #:select? (git-predicate dir)))))))

(define custom-shepherd
  (package
    (inherit *parent-shepherd-package*)
    (version "dev")
    ;; (inputs
    ;;  (modify-inputs (package-inputs *parent-shepherd-package*)
    ;;    (replace "guile-fibers" custom-guile-fibers)))
    (source
     (if *custom-shepherd-only-from-commits*
         (git-checkout
          (url (string-append "file://" *custom-shepherd-dir*))
          (branch "attila")
          ;;(commit "a281ebfd4466a6925e264c303201b8dc44d33cb4")
          )
         (local-file *custom-shepherd-dir*
                     #:recursive? #t
                     #:select? (git-predicate *custom-shepherd-dir*))))
    (arguments
     `(#:tests? #false
       ,@(package-arguments *parent-shepherd-package*)))))

(define *mainnet-group*
  (user-group
   (name "mainnet")
   (system? #t)))

(define *gnosis-user*
  (user-account
   (name "gnosis")
   (group (user-group-name *mainnet-group*))
   ;;(supplementary-groups '("nethermind" "lighthouse"))
   (system? #t)
   (comment (string-append "User for the Gnosis chain daemons"))
   (home-directory "/nohome")
   (shell (file-append shadow "/sbin/nologin"))))

(define *swarm-os*
  (operating-system
   (inherit %simple-os)
   (users (cons*
           *gnosis-user*
           %base-user-accounts))
   (groups (cons*
            *mainnet-group*
            %base-groups))
   (services
    (cons*
     (service dhcp-client-service-type)

     (service
      nethermind-service-type
      (nethermind-service-configuration
       (service-name            'gnosis-e)
       (user-account            *gnosis-user*)
       (nethermind-configuration
        (nethermind-configuration
         (config                "gnosis")
         ;;(Sync.FastSync         #true)
         (JsonRpc.JwtSecretFile "/var/lib/nethermind/jwt-secret")
         (extra-arguments       '())))))

     (service
      lighthouse-service-type
      (lighthouse-service-configuration
       (service-name            'gnosis-c)
       (user-account            *gnosis-user*)
       (requirement             '(gnosis-e))
       (lighthouse-configuration
        (lighthouse-configuration
         (network               "gnosis")
         (logfile-compress      #true)
         (extra-arguments       '((http #true)
                                  (execution-endpoint "http://localhost:8551")
                                  (execution-jwt "/var/lib/nethermind/jwt-secret")
                                  (checkpoint-sync-url "https://checkpoint.gnosischain.com")))))))

     (service
      swarm-service-type
      (swarm-service-configuration
       (swarm                           swarm/mainnet)
       (node-count                      2)
       ;;(requirement '(gnosis))
       (bee-configuration
        (bee-configuration
         (resolver-options               "https://mainnet.infura.io/v3/7216e0c44a0c47d99ca6de1c82b5d7b9")
         ;;(blockchain-rpc-endpoint        "/var/lib/openethereum/gnosis/gnosis.ipc")
         ;;(blockchain-rpc-endpoint        "https://gno.getblock.io/30f88254-c3fb-4188-a991-060f50085ecb/mainnet/")
         (blockchain-rpc-endpoint        "http://serlap.lan:8545")
         (swap-initial-deposit 0)
         (debug-api-enable     #true)))))

     (modify-services %base-services
       (sysctl-service-type config =>
                            (sysctl-configuration
                             (settings (append '(("fs.file-max" . "500000")
                                                 ("fs.inotify.max_user_watches" . "524288"))
                                               %default-sysctl-settings)))))))
   ;; Use own Shepherd package.
   (essential-services
    (modify-services (operating-system-default-essential-services
                      this-operating-system)
                     (shepherd-root-service-type
                      config =>
                      (shepherd-configuration
                       (inherit config)
                       (shepherd (if *use-custom-shepherd*
                                     custom-shepherd
                                     shepherd))))))))

(define *swarm-marionette-os*
  (marionette-operating-system
   (operating-system
     (inherit *swarm-os*)
     (kernel-arguments '())
     (packages (append
                (list
                 ;; netcat-openbsd
                 socat
                 ;; for HTTPS access
                 nss-certs
                 le-certs)
                %base-packages)))
   #:requirements '(nscd)
   #:imported-modules '((gnu services herd)
                        (guix combinators))))


(define* (run-basic-test os command #:optional (derivation-name "swarm-service")
                         #:key
                         initialization
                         root-password
                         desktop?)

  (define test-expression
    (with-imported-modules '((gnu build marionette)
                             (guix build syscalls))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build syscalls)
                       (srfi srfi-1)
                       (srfi srfi-26)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette #$command))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "swarm-service")

          #$(and initialization
                 (initialization #~marionette))

          ;; Shepherd reads the config file *before* binding its control
          ;; socket, so /var/run/shepherd/socket might not exist yet when the
          ;; 'marionette' service is started.
          (test-assert "shepherd socket ready"
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (let loop ((i 10))
                  (cond ((file-exists? (%shepherd-socket-file))
                         #t)
                        ((> i 0)
                         (sleep 1)
                         (loop (- i 1)))
                        (else
                         #f))))
             marionette))

          (test-assert "accounts"
            (let ((users (marionette-eval '(begin
                                             (use-modules (ice-9 match))
                                             (let loop ((result '()))
                                               (match (getpwent)
                                                 (#f (reverse result))
                                                 (x  (loop (cons x result))))))
                                          marionette)))
              (lset= equal?
                     (map (lambda (user)
                            (list (passwd:name user)
                                  (passwd:dir user)))
                          users)
                     (list
                      #$@(map (lambda (account)
                                `(list ,(user-account-name account)
                                       ,(user-account-home-directory account)))
                              (operating-system-user-accounts os))))))

          (test-assert "shepherd services"
            (let ((services (marionette-eval
                             '(begin
                                (use-modules (gnu services herd))

                                (map (compose car live-service-provision)
                                     (current-services)))
                             marionette)))
              (lset= eq?
                     (pk 'services services)
                     '(root #$@(operating-system-shepherd-service-names os)))))

          (test-equal "login on tty1"
            "root\n"
            (begin
              ;; XXX: On desktop, GDM3 will switch to TTY7. If this happens
              ;; after we switched to TTY1, we won't be able to login. Make
              ;; sure to wait long enough before switching to TTY1.
              (when #$desktop?
                (sleep 30))

              (marionette-control "sendkey ctrl-alt-f1" marionette)
              ;; Wait for the 'term-tty1' service to be running (using
              ;; 'start-service' is the simplest and most reliable way to do
              ;; that.)
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'term-tty1))
               marionette)

              ;; Now we can type.
              (let ((password #$root-password))
                (if password
                    (begin
                      (marionette-type "root\n" marionette)
                      (wait-for-screen-text marionette
                                            (lambda (text)
                                              (string-contains text "Password"))
                                            #:ocrad
                                            #$(file-append ocrad "/bin/ocrad"))
                      (marionette-type (string-append password "\n\n")
                                       marionette))
                    (marionette-type "root\n\n" marionette)))
              (marionette-type "id -un > logged-in\n" marionette)

              ;; It can take a while before the shell commands are executed.
              (marionette-eval '(use-modules (rnrs io ports)) marionette)
              (wait-for-file "/root/logged-in" marionette
                             #:read 'get-string-all)))

          (test-assert "host name resolution"
            (match (marionette-eval
                    '(begin
                       ;; Wait for nscd or our requests go through it.
                       (use-modules (gnu services herd))
                       (start-service 'nscd)

                       (list (getaddrinfo "localhost")
                             (getaddrinfo #$(operating-system-host-name os))))
                    marionette)
              ((((? vector?) ..1) ((? vector?) ..1))
               #t)
              (x
               (pk 'failure x #f))))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation derivation-name test-expression))

(define *test-swarm-starting*
  (system-test
   (name "swarm-service-start")
   (description
    "Instrument *SWARM-MARIONETTE-OS* to run it in a VM, and test the SWARM-SERVICE-TYPE.")
   (value
    (run-basic-test (virtualized-operating-system *swarm-marionette-os* '())
                    #~(list #$(virtual-machine *swarm-marionette-os*))))))

;; Return the OS object, so that it can be passed to `guix system vm swarm.scm`.
*swarm-marionette-os*
