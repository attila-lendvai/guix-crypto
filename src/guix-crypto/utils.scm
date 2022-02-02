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

(define-module (guix-crypto utils)
  ;; CALL-WITH-PORT is only available from (ice-9 ports) after commit
  ;; 9fecf20fcf1bac764b3d812e07ed4a4a56be52a2 (which was released in Guile
  ;; 3.0.6). Prior to that it was only in (scheme base).
  #:use-module (guix build utils)
  #:use-module ((scheme base) #:select (call-with-port))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format))

;;;
;;; Logging
;;;
(define +log-directory-initial-value+
  "BUG: Swarm's *LOG-DIRECTORY* is unset")

(define-public *log-directory* (make-parameter +log-directory-initial-value+))

(define-public (service-log-filename)
  (let ((basedir (*log-directory*)))
    (when (string= basedir +log-directory-initial-value+)
      (let ((port (current-error-port)))
        (format port "ERROR: SERVICE-LOG-FILENAME called with *LOG-DIRECTORY* uninitialized:")
        (false-if-exception (display-backtrace (make-stack #t) port))))
    (string-append basedir "/service.log")))

(define-public (log.debug format-string . args)
  ;; WITH-OUTPUT-TO-FILE doesn't work here, because we need to
  ;; append, and it overwrites.
  (call-with-port (open-file (service-log-filename) "a")
                  (lambda (port)
                    (display (date->string (current-date) "~5") port)
                    (display #\space port)
                    (apply format port format-string args)
                    (newline port)))
  (values))

(define-public (log.dribble . args)
  (apply log.debug args))

(define-public (log.error . args)
  (apply log.debug args))

;;;
;;; Shell and file stuff
;;;
(define-public (invoke-as-user pw thunk)
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

(define-public (read-file-to-string path)
  (call-with-input-file path
    (lambda (port)
      (get-string-all port))))

(define-public (chown-r uid gid . dirs)
  (for-each (if uid
                (cute invoke "chown" "-R" (format #f "~A:~A" uid gid) <>)
                (cute invoke "chgrp" "-R" (format #f "~A" gid) <>))
            dirs))

(define-public (ensure-password-file password-file uid gid)
  (log.dribble "ENSURE-PASSWORD-FILE-EXISTS for ~S" password-file)
  (unless (file-exists? password-file)
    (log.debug "Generating password file ~S" password-file)
    (let ((cmd (string-append
                "< /dev/urandom tr -dc _A-Z-a-z-0-9 2> /dev/null | head -c32 >'"
                password-file "'")))
      (unless (zero? (system cmd))
        (error "Failed to generate password file" password-file))
      (chmod password-file #o400)))
  (chown password-file (or uid -1) (or gid -1)))

(define-public (ensure-directories owner group permissions . directories)
  (let ((uid (if (string? owner)
                 (passwd:uid (getpwnam owner))
                 owner))
        (gid (if (string? group)
                 (group:gid (getgrnam group))
                 group)))
    (for-each
     (lambda (dir)
       (mkdir-p dir)
       (when permissions
         (chmod dir permissions))
       (chown dir (or uid -1) (or gid -1)))
     directories)))
