;;; Copyright © 2023 Attila Lendvai <attila@lendvai.name>
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

;;;
;;; This file does not bring in a large transitive closure of
;;; dependencies (i.e. no dependency on GEXP stuff), and thus can be
;;; used in the Shephard side of the code (e.g. in the service start
;;; forms), and also on the builder side.
;;;

(define-module (guix-crypto swarm-utils)
  #:use-module (guix-crypto utils)
  #:use-module (guix build utils)
  #:use-module ((scheme base) #:select (call-with-port))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  ;; TODO when we #:use-module (fibers), then guix system vm fails to build
  ;;#:use-module ((fibers) #:hide (sleep))
  #:use-module (json)
  #:export        ; Also note the extensive use of DEFINE-PUBLIC below
  ())

(define-public *service-log-directory* "/var/log/swarm/")

(define-public (default-log-directory swarm-name)
  (string-append *service-log-directory* swarm-name))

(define-public (bee-log-filename log-dir bee-index)
  (simple-format #f "~A/bee-~A.log" log-dir bee-index))

(define-public *service-data-directory* "/var/lib/swarm/")

(define-public (swarm-data-directory swarm-name)
  (string-append *service-data-directory* swarm-name))

(define-public (bee-data-directory swarm-name bee-index)
  (string-append (swarm-data-directory swarm-name) "/bee-"
                 (number->string bee-index)))

(define-public (bee-wallet-file swarm-name bee-index)
  (string-append (bee-data-directory swarm-name bee-index) "/keys/swarm.key"))

(define-public (clef-data-directory swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef"))

(define-public (clef-ipc-file swarm-name)
  (string-append (clef-data-directory swarm-name) "/clef.ipc"))

(define-public (clef-keystore-directory swarm-name)
  (string-append (clef-data-directory swarm-name) "/keystore"))

(define-public (bee-password-file swarm-name bee-index)
  (string-append (swarm-data-directory swarm-name) "/bee-"
                 (number->string bee-index) ".password"))

(define-public (clef-password-file swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef-password"))

(define-public (clef-service-name swarm-name)
  (string->symbol (simple-format #f "clef-~A" swarm-name)))

(define-public (bee-service-name swarm-name bee-index)
  (string->symbol (simple-format #f "bee-~A-~A" swarm-name bee-index)))

;; Clef command list:
;; https://github.com/ethereum/go-ethereum/blob/master/signer/core/stdioui.go
;; https://github.com/ethereum/go-ethereum/blob/master/signer/core/uiapi.go
;; https://github.com/ethereum/go-ethereum/blob/master/signer/core/cliui.go
;; https://github.com/ethereum/go-ethereum/blob/master/cmd/clef/datatypes.md

(define-public* (spawn-clef-stdio-fiber pid input output clef-password)
  (log.debug "CLEF-STDIO-LOOP is speaking")

  ;; TODO use this? what about the fiber capturing this closure?
  ;; (define (cleanup . _)
  ;;   (close-port input)
  ;;   (close-port output)
  ;;   #false)

  (catch 'quit
    (lambda _
      (define (quit)
        (throw 'quit #false))

      (define (assert-clef-is-alive)
        ;; (log.dribble "is clef alive on pid ~S?" pid)
        (unless (or (not pid)
                    (is-pid-alive? pid))
          (log.debug "pid ~S is gone, fiber is exiting..." pid)
          (quit)))

      (define (next-line)
        (assert-clef-is-alive)
        ;; (log.debug "entering read-line")
        (let ((line (read-line input)))
          (log.dribble "Got line ~S" line)
          (if (eof-object? line)
              (begin
                (log.debug "Got EOF from Clef, fiber is exiting...")
                (quit))
              line)))

      (define (read-request)
        (let* ((line (next-line))
               ;; TODO avoid using READ-LINE and use JSON->SCM directly on the port
               ;; but it gets stuck for some reason.
               ;; also see other instances of JSON-STRING->SCM
               (json (if (eof-object? line)
                         (begin
                           (log.debug "CLEF-STDIO-LOOP Got EOF from clef, returning...")
                           (quit))
                         (json-string->scm line)))
               (params (assoc-ref json "params")))
          (log.dribble "CLEF-STDIO-LOOP Got request from Clef: ~A" json)
          (values (assoc-ref json "method")
                  (if params (vector-ref params 0) '())
                  (assoc-ref json "id")
                  json)))

      (define (respond fmt . args)
        (assert-clef-is-alive)
        (let ((response (apply format #false fmt args)))
          ;; TODO don't log the content itself
          (log.dribble "CLEF-STDIO-LOOP Sending response to Clef ~A" response)
          (put-string output response))
        (newline output)
        (force-output output))

      (define (respond-text id value)
        (respond "{ \"jsonrpc\": \"2.0\", \"id\":~A, \"result\": { \"text\":\"~A\" } }"
                 id value))

      (define (respond-bool id name value)
        (respond "{ \"jsonrpc\": \"2.0\", \"id\":~A, \"result\": { \"~A\": ~A } }"
                 id name (if value "true" "false")))

      (define (respond-json id json)
        (let ((json (cons* (cons "id" id)
                           '("jsonrpc" . "2.0")
                           json)))
          (log.debug "CLEF-STDIO-LOOP responding with json ~S" json)
          (respond (scm->json-string json))))

      (log.dribble "CLEF-STDIO-LOOP is waiting for the Master Password prompt")

      (let* ((method params id _ (read-request))
             (title (assoc-ref params "title")))
        (if (and (equal? method "ui_onInputRequired")
                 (equal? title "Master Password"))
            (begin
              (log.debug "CLEF-STDIO-LOOP Answering master password")
              (respond-text id clef-password))
            (error "Clef didn't ask for the master password?!")))

      (let ((method (read-request)))
        (if (equal? method "ui_onSignerStartup")
            ;; {"jsonrpc":"2.0","method":"ui_onSignerStartup","params":[{"info":{"extapi_http":"n/a","extapi_ipc":"/var/lib/swarm/mainnet/clef/clef.ipc",}
            (log.debug "CLEF-STDIO-LOOP Signer has started up")
            (error "Clef didn't send ui_onSignerStartup?!")))

      ;; we only spawn the fiber (and return) once Clef has finished starting up.
      ((@ (fibers) spawn-fiber) ; TODO should be just (spawn-fiber ...), but see TODO at the module def
       (lambda ()
         (catch 'quit
           (lambda _
             (log.debug "stdio loop fiber speaking")
             (let* ((handlers
                     (list
                      ;; new account password
                      (cons (lambda (method params id request)
                              (let ((title (assoc-ref params "title")))
                                (and (equal? method "ui_onInputRequired")
                                     (or (equal? title "New account password")
                                         (equal? title "Password for signing")))))

                            (lambda (method params id . _)
                              (log.debug "CLEF-STDIO-LOOP Answering new account password for request id ~A" id)
                              (respond-text id clef-password)))

                      ;; ignore some messages
                      (cons (lambda (method . _)
                              (member method '("ui_onApprovedTx")))

                            (lambda (method params id . _)
                              (values)))

                      ;; approve some functionality
                      (cons (lambda (method . _)
                              (member method '("ui_approveNewAccount"
                                               "ui_approveSignData")))

                            (lambda (method params id . _)
                              (log.debug "CLEF-STDIO-LOOP approving method ~A, id ~A" method id)
                              (respond-bool id "approved" #true)))

                      ;; ui_approveListing must answer with a list of accounts
                      (cons (lambda (method . _)
                              (equal? method "ui_approveListing"))

                            (lambda (method params id request)
                              (log.debug "CLEF-STDIO-LOOP approving method ~A, id ~A" method id)
                              (respond-json id `((result . ((accounts . ,(assoc-ref params "accounts"))))))))

                      ;; ui_approveTx must answer with the transaction
                      (cons (lambda (method . _)
                              (equal? method "ui_approveTx"))

                            (lambda (method params id request)
                              (log.debug "CLEF-STDIO-LOOP approving method ~A, id ~A" method id)
                              (respond-json id `((result . ((approved . #true)
                                                            (transaction . ,(assoc-ref params "transaction"))))))))

                      (cons (const #true)
                            (lambda (method params id request)
                              (log.warn "Clef said something unexpected: ~A" request))))))
               (log.debug "Entering the stdio loop with clef")
               (let loop ()
                 (let* ((method params id request (read-request)))
                   (log.debug "CLEF-STDIO-LOOP Clef said '~A'" request)

                   ;; KLUDGE FOR-EACH is not (guaranteed) to be a tail call,
                   ;; so we need to roll our own. this is kinda ugly. it
                   ;; should really be just a for-each and a break, but THROW
                   ;; flies out of the fiber.
                   (define (try-handlers handlers)
                     (unless (null? handlers)
                       (let* ((entry   (first handlers))
                              (pred    (car entry))
                              (handler (cdr entry)))
                         ;;(log.dribble "CLEF-STDIO-LOOP trying pred '~A'" pred)
                         (if (pred method params id request)
                             (begin
                               ;;(log.dribble "CLEF-STDIO-LOOP calling handler '~A'" handler)
                               (handler method params id request))
                             (try-handlers (cdr handlers))))))

                   (try-handlers handlers)

                   (loop)))))
           (const #false)))))
    (const #false)))

(define-public* (ensure-clef-account swarm-name bee-index)
  (call-with-port (socket PF_UNIX SOCK_STREAM 0)
    (lambda (s)
      (connect s AF_UNIX (clef-ipc-file swarm-name))
      (make-port-non-blocking! s)
      (ensure-clef-account* s swarm-name bee-index))))

(define-public* (ensure-clef-account* socket swarm-name bee-index)
  (log.debug "ENSURE-CLEF-ACCOUNT called for bee-index ~A" bee-index)
  (let/ec quit
    (let ((input socket)
          (output socket))

      (define (next-line)
        (let ((line (read-line input)))
          ;; TODO don't log the content itself
          (log.debug "Got line ~A" line)
          (if (eof-object? line)
              (begin
                (log.debug "Got EOF, exiting...")
                (quit #false))
              line)))

      (define (request/text fmt . args)
        (let ((msg (apply format #false fmt args)))
          (log.debug "Sending request to Clef ~S" msg)
          (put-string output msg))
        ;;(newline pipe)
        (force-output output)

        (let ((msg (read-response)))
          (log.debug "Got response from Clef ~A" msg)
          (values (assoc-ref msg "result")
                  msg)))

      (define (read-response)
        (let* ((line (next-line))
               (json (if (eof-object? line)
                         (begin
                           (log.debug "Got EOF from clef, returning...")
                           (quit #false))
                         (json-string->scm line))))
          json))

      (let* ((id -1)
             (next-id (lambda ()
                        (set! id (1+ id))
                        id)))

        (define (request method . args)
          (assert (= 0 (length args)))
          (request/text "{\"id\": ~A, \"jsonrpc\": \"2.0\", \"method\": \"~A\"}"
                        (next-id) method))

        ;; we only create one new account at a time, because other starting up
        ;; bee nodes may be running this same loop in parallel.
        ;; the worst race condition here is that we generate one more
        ;; clef account than necessary, which is not a big deal.
        (while (let* ((accounts (request "account_list"))
                      (num-accounts (vector-length accounts)))
                 (<= num-accounts bee-index))
          (request "account_new"))

        ;; fetch the account address and return it.
        (let* ((accounts (request "account_list"))
               (address (vector-ref accounts bee-index)))
          (log.debug "Account address for bee-index ~A is ~A" bee-index address)
          (assert (= 42 (string-length address)))
          (substring address 2))))))
