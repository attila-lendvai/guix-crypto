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
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto service-utils)
  #:use-module (guix-crypto packages ethereum)
  #:use-module (guix-crypto packages swarm)
  #:use-module (guix-crypto services swarm)
  #:use-module (guix-crypto services lighthouse)
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
(define *parent-shepherd-package* shepherd-0.9)
(define *custom-shepherd-dir* "/home/alendvai/workspace/guix/shepherd")
(define *custom-shepherd-only-from-commits* #false)

(define custom-shepherd
  (package
    (inherit *parent-shepherd-package*)
    (version "dev")
    (source
     (if *custom-shepherd-only-from-commits*
         (git-checkout
          (url (string-append "file://" *custom-shepherd-dir*))
          (branch "attila")             ; not optional
          ;;(commit "607f38e5104b9b03e020cd071535a840b4168e11")
          )
         (local-file *custom-shepherd-dir*
                     #:recursive? #t
                     #:select? (git-predicate *custom-shepherd-dir*))))
    (arguments
     `(#:tests? #false
       ,@(package-arguments *parent-shepherd-package*)))))

(define *swarm-os*
  (operating-system
    (inherit %simple-os)
    ;; (users (cons* (user-account
    ;;                (name "gnosis")
    ;;                (comment "Gnosis endpoint user")
    ;;                (group "swarm-mainnet")
    ;;                (system? #true))
    ;;               %base-user-accounts))
    ;; (groups (append
    ;;          (list
    ;;           (user-group
    ;;            ;; to make sure it's present even when the swarm service is disabled
    ;;            (name "swarm-mainnet")
    ;;            (system? #true)))
    ;;          %base-groups))
    (services
     (cons*
      (service dhcp-client-service-type)

      ;; (swarm-service #:node-count 2
      ;;                #:blockchain-rpc-endpoint "/var/lib/openethereum/gnosis/gnosis.ipc"
      ;;                #:bee-supplementary-groups '("openethereum")
      ;;                #:swap-initial-deposit 0
      ;;                #:dependencies '(gnosis))

      (service
       swarm-service-type
       (swarm-service-configuration
        (swarm                           swarm/mainnet)
        (node-count                      2)
        (shepherd-requirement '(gnosis))
        (bee-configuration
         (bee-configuration
          (blockchain-rpc-endpoint        "/var/lib/openethereum/gnosis/gnosis.ipc")
          (swap-initial-deposit 0)
          (debug-api-enable     #true)))))

      (lighthouse-service #:service-name   'gnosis
                          #:network        "gnosis"
                          #:user           "gnosis"
                          #:enable-snapshotting #true
                          #:warp-barrier   20420000)

      ;; (openethereum-service #:service-name 'gnosis
      ;;                       #:chain        "xdai"
      ;;                       #:user         "gnosis"
      ;;                       #:group        "swarm-mainnet"
      ;;                       #:snapshot-peers 10
      ;;                       #:enable-snapshotting #true
      ;;                       #:warp-barrier 20420000)

      ;; (service openethereum-service-type
      ;;          (openethereum-service-configuration
      ;;           (service-name            'xdai)
      ;;           (user                    "xdai")
      ;;           (group                   "swarm-mainnet")
      ;;           (openethereum-configuration
      ;;            (openethereum-configuration
      ;;             (chain                 "xdai")
      ;;             (nat                   "upnp")
      ;;             (max-peers             50)
      ;;             (snapshot-peers        10)
      ;;             (warp-barrier          20400000)
      ;;             (scale-verifiers       #true)))))

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
       (shepherd-root-service-type config =>
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
                 netcat-openbsd
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

  (define guix&co
    (match (package-transitive-propagated-inputs guix)
      (((labels packages) ...)
       (cons guix packages))))

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
