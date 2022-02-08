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

;;;
;;; This file is rather liberal in depending on other modules, and
;;; mostly containst stuff that is useful for the service
;;; implementation, but only on the build side (i.e. not in code that
;;; will be executed by Shephard).
;;;

(define-module (guix-crypto service-utils)
  #:use-module (guix-crypto utils)
  #:use-module (gnu packages base)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  ;;#:use-module (guix utils)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export        ; Also note the extensive use of DEFINE-PUBLIC below
  (with-service-gexp-modules))

;;;
;;; Configuration
;;;
(define-public (undefined-value? x)
  (or (eq? x 'undefined)
      (eq? x 'disabled)))

(define-public (defined-value? x)
  (if (undefined-value? x)
      #false
      x))

(define-public (non-negative-integer? val)
  (and (exact-integer? val)
       (not (negative? val))))

;;;
;;; Service stuff
;;;
(define-public +default-service-modules+
  (append '((gnu build shepherd)
            (guix-crypto utils))
          ;; This %default-modules is from Shepherd.
          %default-modules))

(define-public default-service-module-filter
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (('nongnu _ ...) #t)
    (('nonguix _ ...) #t)
    (('guix-crypto _ ...) #t)
    (_ #f)))

(define-syntax-rule (with-service-gexp-modules modules body ...)
  (with-imported-modules (source-module-closure
                          (append +default-service-modules+
                                  modules)
                          #:select? default-service-module-filter)
    body ...))