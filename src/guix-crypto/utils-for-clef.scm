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

(define-module (guix-crypto utils-for-clef)
  #:use-module (guix-crypto utils)
  #:use-module (guix build utils)
  #:use-module ((scheme base) #:select (call-with-port))
  ;;#:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export        ; Also note the extensive use of DEFINE-PUBLIC below
  ())

;; Clef command list: https://github.com/ethereum/go-ethereum/blob/master/signer/core/uiapi.go

(define-public* (clef-stdio-loop pipe clef-password node-count)
  (log.debug "CLEF-STDIO-LOOP is speaking")
  (let* ((id 0)
         (next-line (lambda ()
                      (let loop ((line (read-line pipe)))
                        (log.debug "Got line ~S" line)
                        (if (equal? "" line)
                            (loop (read-line pipe))
                            line))))
         (respond (lambda (fmt . args)
                    (let ((response (apply format #false fmt args)))
                      (log.debug "Will respond ~S" response)
                      (put-string pipe response))
                    ;;(newline pipe)
                    (force-output pipe)))
         (next-id (lambda ()
                    (set! id (1+ id))
                    id)))
    (let* ((master-password-rx (make-regexp "\"id\":([0-9]+).*ui_onInputRequired.*Master Password"))
           (hit (regexp-exec master-password-rx (next-line))))
      (if hit
          (begin
            (set! id (string->number (match:substring hit 1)))
            (log.debug "Answering master password")
            (respond "{ \"jsonrpc\": \"2.0\", \"id\":~A, \"result\": { \"text\":\"~A\" } }"
                     id clef-password))
          (error "Clef didn't ask for the master password?!")))

    (let ((signer-startup-rx (make-regexp "ui_onSignerStartup")))
      (if (regexp-exec signer-startup-rx (next-line))
          (log.debug "Signer has started up")
          (error "Clef didn't send ui_onSignerStartup?!")))

    (log.debug "Querying clef account_list")
    (respond "{\"id\": ~A, \"jsonrpc\": \"2.0\", \"method\": \"clef_listAccounts\"}"
             (next-id))

    ;; Let's ensure that we have at least NODE-COUNT clef accounts
    (let ((account-response (next-line)))
      (log.debug "The account list: ~S" account-response)
      ;;(hash-result-regexp (make-regexp "\"result\":\"0x([0-9a-fA-F]{40})\""))
      )

    ;; Log everything else as a warning
    (let loop ((response (next-line)))
      (log.warn "Unexpected communication from Clef: ~S" response)
      (loop (next-line)))))

#;(define-public* (clef-stdio-loop pipe clef-password)
  (log.debug "CLEF-STDIO-LOOP is speaking")
  (let* ((next-line (lambda ()
                      (read-line pipe)))
         (hash-result-regexp (make-regexp "\"result\":\"0x([0-9a-fA-F]{40})\""))
         (matchers
          (list
           (cons (make-regexp "\"id\":([0-9]+).*ui_onInputRequired.*Master Password")
                 (lambda (hit)
                   (let ((id (match:substring hit 1)))
                     (log.debug "Answering master password")
                     (format pipe "{ \"jsonrpc\": \"2.0\", \"id\":~A, \"result\": { \"text\":\"~A\" } }"
                             id clef-password))))
           (cons (make-regexp "\"id\":([0-9]+).*ui_onInputRequired.*New account password")
                 (lambda (hit)
                   (let ((id (match:substring hit 1)))
                     (log.debug "Answering new account password")
                     (format pipe "{ \"jsonrpc\": \"2.0\", \"id\":~A, \"result\": { \"text\":\"~A\" } }"
                             id clef-password)
                     (let ((response (next-line)))
                       (if (set! hit (regexp-exec hash-result-regexp response))
                           (log.debug "Successfully created account 0x~A" (match:substring hit 1))
                           (log.debug "Failed to create account: ~S" response))))))
           (cons (make-regexp ".*")
                 (lambda (hit)
                   (error "Clef said something unexpected:" (match:substring hit)))))))
    (let loop ((line (next-line)))
      (unless (eof-object? line)
        (log.debug "Clef said '~A'" line)
        (for-each
         (match-lambda
           ((regexp . handler)
            (let ((hit (regexp-exec regexp line)))
              (when hit
                (handler hit)
                (loop (next-line))))))
         matchers)
        (error "This shouldn't have been reached")))))
