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

(define-module (guix-crypto service-utils)
  #:use-module (guix-crypto utils)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match))

;;;
;;; Configuration
;;;
(define-public (undefined-value? x)
  (or (eq? x 'undefined)
      (eq? x 'disabled)))

(define-public (defined-value? x)
  (not (undefined-value? x)))

(define-public (non-negative-integer? val)
  (and (exact-integer? val)
       (not (negative? val))))

;;;
;;; Service stuff
;;;
(define-public default-service-module-filter
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (('nongnu _ ...) #t)
    (('nonguix _ ...) #t)
    (('guix-crypto _ ...) #t)
    (_ #f)))

;; NOTE using DEFINE-PUBLIC* here results in an undefined function
;; error below, probably because BEGIN breaking toplevelness.
(define* (wait-for-file pid path #:optional (timeout 60))
  (false-if-exception
   (delete-file path))
  (let ((start (get-monotonic-time))
        (time-passed 0)
        (pid-pair #f))
    (while
        (begin
          (format #t "Waiting for the file '~S' to show up; ~F secs passed.~%" path time-passed)
          (log.debug "Waiting for the file '~S' to show up; ~F secs passed." path time-passed)
          (sleep 1)
          (set! time-passed (- (get-monotonic-time) start))
          (set! pid-pair    (waitpid pid WNOHANG))
          (let ((child-exited? (not (zero? (car pid-pair)))))
            (and (< time-passed timeout)
                 (not child-exited?)
                 (not (file-exists? path))))))
    (values time-passed (car pid-pair) (cdr pid-pair))))

(define-public* (ensure-ipc-file-permissions pid path #:optional (perms #o660))
  (let ((time-passed child-pid exit-code (wait-for-file pid path)))
   (if (file-exists? path)
       (begin
         (log.debug "Setting permissions of file '~S' to #o~O" path perms)
         (chmod path perms))
       (log.error "Unexpected outcome while waiting for file '~S'; child-pid is ~S, exit-code is ~S, time-passed is ~F" path child-pid exit-code time-passed))))
