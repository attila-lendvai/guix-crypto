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

(define-module (guix-crypto services swarm-utils)
  ;; CALL-WITH-PORT is only available from (ice-9 ports) after commit
  ;; 9fecf20fcf1bac764b3d812e07ed4a4a56be52a2 (which was released in Guile
  ;; 3.0.6). Prior to that it was only in (scheme base).
  #:use-module (guix build utils)
  #:use-module ((scheme base) #:select (call-with-port))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export
  (*log-directory*
   default-log-directory
   service-log-filename
   bee-log-file
   clef-log-file
   log.dribble
   log.debug
   log.error

   swarm-data-directory
   bee-data-directory
   bee-account-file
   clef-data-directory
   clef-ipc-file
   clef-keystore-directory
   bee-password-file
   clef-password-file
   clef-service-name
   bee-service-name

   invoke-as-user
   read-file-to-string
   chown-r
   ))

;;;
;;; Logging
;;;
(define +log-directory-initial-value+
  "BUG: Swarm's *LOG-DIRECTORY* is unset")

(define *log-directory* (make-parameter +log-directory-initial-value+))

(define (default-log-directory swarm-name)
  (string-append "/var/log/swarm/" swarm-name))

(define (bee-log-file swarm-name bee-index)
  (string-append (*log-directory*)
                 "/bee-" (number->string bee-index) ".log"))

(define (clef-log-file swarm-name)
  (string-append (*log-directory*) "/clef.log"))

(define (service-log-filename)
  (let ((basedir (*log-directory*)))
    (when (string= basedir +log-directory-initial-value+)
      (let ((port (current-error-port)))
        (format port "ERROR: SERVICE-LOG-FILENAME called with *LOG-DIRECTORY* uninitialized:")
        (false-if-exception (display-backtrace (make-stack #t) port))))
    (string-append basedir "/service.log")))

(define (log.debug format-string . args)
  ;; WITH-OUTPUT-TO-FILE doesn't work here, because we need to
  ;; append, and it overwrites.
  (call-with-port (open-file (service-log-filename) "a")
                  (lambda (port)
                    (display (date->string (current-date) "~5") port)
                    (display #\space port)
                    (apply format port format-string args)
                    (newline port)))
  (values))

(define (log.dribble . args)
  (apply log.debug args))

(define (log.error . args)
  (apply log.debug args))

;;;
;;; Data dir
;;;
(define +service-data-directory+ "/var/lib/swarm/")

(define (swarm-data-directory swarm-name)
  (string-append +service-data-directory+ swarm-name))

(define (bee-data-directory swarm-name bee-index)
  (string-append (swarm-data-directory swarm-name) "/bee-"
                 (number->string bee-index)))

(define (bee-account-file swarm-name bee-index)
  (string-append (bee-data-directory swarm-name bee-index) "/eth-address"))

(define (clef-data-directory swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef"))

(define (clef-ipc-file swarm-name)
  (string-append (clef-data-directory swarm-name) "/clef.ipc"))

(define (clef-keystore-directory swarm-name)
  (string-append (clef-data-directory swarm-name) "/keystore"))

(define (bee-password-file swarm-name)
  (string-append (swarm-data-directory swarm-name) "/bee-password"))

(define (clef-password-file swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef-password"))

(define (clef-service-name swarm-name)
  (string->symbol (simple-format #f "clef-~A" swarm-name)))

(define (bee-service-name swarm-name bee-index)
  (string->symbol (simple-format #f "bee-~A-~A" swarm-name bee-index)))

(define (invoke-as-user pw thunk)
  (log.dribble "INVOKE-AS-USER called with pw ~S" pw)
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        (dynamic-wind
          (const #t)
          (lambda ()
            (log.dribble "INVOKE-AS-USER is inside the fork")
            ;; Note that we can't log after the SETGID, because the file might
            ;; be read-only for us.
            (setgid (passwd:gid pw))
            (setuid (passwd:uid pw))
            (umask #o007)
            (with-exception-handler
                (lambda (e)
                  (log.error "INVOKE-AS-USER thunk threw an error: ~A" e))
              thunk)
            (primitive-exit 0))
          (lambda ()
            (log.dribble "INVOKE-AS-USER is exiting the fork with an error")
            ;; Exit with a non-zero status code if an exception is thrown.
            (primitive-exit 1)))
        (begin
          (log.dribble "INVOKE-AS-USER is waiting for the fork")
          (let ((exit-code (cdr (waitpid pid))))
            (log.dribble "INVOKE-AS-USER fork has finished with exit-code ~S" exit-code)
            (unless (zero? exit-code)
              (log.error "INVOKE-AS-USER has exited with code ~S" pw exit-code)
              (error "INVOKE-AS-USER failed for" pw)))))))

(define (read-file-to-string path)
  (call-with-input-file path
    (lambda (port)
      (get-string-all port))))

(define (chown-r dir uid gid)
  (let ((spec (format #f "~A:~A" uid gid)))
    (invoke "chown" "-R" spec dir)))

;; (define (ensure-password-file-exists password-file pw)
;;   (log.debug "ensure-password-file-exists for ~S" password-file)
;;   (if (file-exists? password-file)
;;       (invoke "chown" (string-append (passwd:uid pw) ":" (passwd:gid pw))
;;               password-file)
;;       (let ((cmd (string-append
;;                   "< /dev/urandom " tr
;;                   " -dc _A-Z-a-z-0-9 2> /dev/null | " head
;;                   " -c32 >"
;;                   password-file)))
;;         (log.debug "Generating password file ~S" password-file)
;;         (invoke-as-user
;;          pw
;;          (lambda ()
;;            (unless (zero? (system* "/bin/sh" "-c" cmd))
;;              (error "Failed to generate password file" password-file))
;;            (chmod password-file #o400))))))
