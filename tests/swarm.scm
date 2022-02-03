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
;; $(guix system --no-graphic vm ~/workspace/guix/guix-crypto/guix-crypto/tests/swarm.scm) -m 2048
;; $(./pre-inst-env guix system --no-graphic vm ../guix-crypto/guix-crypto/tests/swarm.scm) -m 2048

(define-module (guix-crypto tests swarm)
  #:use-module (guix-crypto packages ethereum)
  #:use-module (guix-crypto packages swarm)
  #:use-module (guix-crypto services swarm)
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tmux)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (run-basic-test
            *test-swarm-starting*))

(define +mainnet-xdai-ipc-file+ "/var/lib/swarm/mainnet/xdai.ipc")

(define *swarm-os*
  (operating-system
    (inherit %simple-os)
    (users (cons* (user-account
                   (name "xdai")
                   (comment "xdai service")
                   (group "swarm-mainnet")
                   (system? #true))
                  %base-user-accounts))
    (groups (append
             (list
              (user-group
               ;; to make sure it's present even when the swarm service is disabled
               (name "swarm-mainnet")
               (system? #true)))
             %base-groups))
    (services
     (cons*
      (service dhcp-client-service-type)

      (swarm-service #:node-count 2
                     #:swap-endpoint +mainnet-xdai-ipc-file+
                     #:swarm 'mainnet
                     #:dependencies '(xdai-mainnet))

      (simple-service
       'xdai-mainnet
       shepherd-root-service-type
       (list
        (shepherd-service
         (documentation "Openethereum node for the xDai chain.")
         (provision '(xdai-mainnet))
         (requirement '(networking file-systems))
         (modules (append '((guix-crypto utils)
                            (guix-crypto service-utils))
                          %default-modules))
         (start
          ;; TODO this WITH-IMPORTED-MODULES shouldn't be needed
          ;; here. adding a module above to the 'modules field of the
          ;; service results in getting imported into the gexps (?),
          ;; but without the w-i-m the module itself is not available.
          ;; shouldn't the 'modules field of the service take care of
          ;; that, too?
          (with-imported-modules '((guix-crypto utils)
                                   (guix-crypto service-utils))
            #~(lambda args
                (setenv "PATH" #$(file-append coreutils "/bin"))
                (ensure-directories #f "swarm-mainnet" #o2770
                                    "/var/log/swarm/mainnet"
                                    "/var/lib/swarm/mainnet"
                                    "/var/lib/swarm/mainnet/xdai")
                (chown-r #f "swarm-mainnet"
                         "/var/log/swarm/mainnet/"
                         "/var/lib/swarm/mainnet/")
                (parameterize ((*log-directory* "/tmp/")) ;; TODO
                  (let* ((ipc-file #$+mainnet-xdai-ipc-file+)
                         (forkexec
                          (make-forkexec-constructor
                           (list #$(file-append openethereum-binary "/bin/openethereum")
                                 "--chain=xdai"
                                 "--scale-verifiers"
                                 "--warp-barrier=20420000"
                                 "--no-ws"
                                 "--no-jsonrpc"
                                 "--base-path=/var/lib/swarm/mainnet/xdai"
                                 "--ipc-path" ipc-file)
                           #:user "xdai"
                           #:group "swarm-mainnet"
                           #:log-file "/var/log/swarm/mainnet/xdai.log"
                           #:directory "/var/lib/swarm/mainnet/xdai"
                           #:environment-variables
                           (list (string-append "PATH=" #$coreutils)
                                 "LC_ALL=en_US.UTF-8"))))
                    (let ((pid (apply forkexec args)))
                      (ensure-ipc-file-permissions pid ipc-file)
                      pid))))))
         (stop #~(make-kill-destructor)))))
      %base-services))))

(define *swarm-marionette-os*
  (marionette-operating-system
   (operating-system
     (inherit *swarm-os*)
     (kernel-arguments '("ip=dhcp"))
     (packages (append
                (list
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
