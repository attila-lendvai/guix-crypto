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
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:export        ; Also note the extensive use of DEFINE-PUBLIC below
  ())

;; Clef command list:
;; https://github.com/ethereum/go-ethereum/blob/master/signer/core/uiapi.go

(define-public* (clef-stdio-loop pid input output clef-password)
  (log.debug "CLEF-STDIO-LOOP is speaking")
  (let/ec quit
    (let* ((id 0)
           (assert-clef-is-alive (lambda ()
                                   ;; (log.dribble "is clef alive on pid ~S?" pid)
                                   (unless (or (not pid)
                                               (is-pid-alive? pid))
                                     (log.debug "pid ~S is gone, fiber is exiting..." pid)
                                     (quit #f))))
           (next-line (lambda ()
                        (assert-clef-is-alive)
                        ;; (log.debug "entering read-line")
                        (let ((line (read-line input)))
                          ;; TODO don't log the content itself
                          (log.debug "Got line ~S" line)
                          (if (eof-object? line)
                              (begin
                                (log.debug "Got EOF from Clef, fiber is exiting...")
                                (quit #f))
                              line))))
           (respond (lambda (fmt . args)
                      (assert-clef-is-alive)
                      (let ((response (apply format #false fmt args)))
                        ;; TODO don't log the content itself
                        (log.debug "Sending response to Clef ~S" response)
                        (put-string output response))
                      ;;(newline pipe)
                      (force-output output)))
           (next-id (lambda ()
                      (set! id (1+ id))
                      id)))
      (log.dribble "CLEF-STDIO-LOOP is waiting for the Master Password prompt")
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

      (let* ((hash-result-regexp (make-regexp "\"result\":\"0x([0-9a-fA-F]{40})\""))
             (matchers
              (list
               (cons (make-regexp "\"id\":([0-9]+).*ui_onInputRequired.*New account password")
                     (lambda (hit)
                       (let ((id (match:substring hit 1)))
                         (log.debug "Answering new account password for request id ~A" id)
                         (format pipe "{ \"jsonrpc\": \"2.0\", \"id\":~A, \"result\": { \"text\":\"~A\" } }"
                                 id clef-password)
                         (let* ((response (next-line))
                                (hit (regexp-exec hash-result-regexp response)))
                           (if hit
                               (log.debug "Successfully created account 0x~A" (match:substring hit 1))
                               (log.debug "Failed to create account: ~S" response))))))
               (cons (make-regexp ".*")
                     (lambda (hit)
                       (error "Clef said something unexpected:" (match:substring hit)))))))
        (let loop ((request (next-line)))
          (unless (eof-object? request)
            (log.debug "Clef said '~A'" request)
            (for-each
             (match-lambda
               ((regexp . handler)
                (let ((hit (regexp-exec regexp request)))
                  (when hit
                    (handler hit)
                    (loop (next-line))))))
             matchers)
            (log.error "This shouldn't have been reached; clef said: ~S" request)
            (error "This shouldn't have been reached; clef said:" request)))))))
