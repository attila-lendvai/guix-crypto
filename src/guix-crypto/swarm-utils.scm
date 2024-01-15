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
  #:use-module (json)
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

(define-public *service-log-directory* "/var/log/swarm")

(define-public (default-log-directory swarm-name)
  (simple-format #f "~A/~A" *service-log-directory* swarm-name))

(define-public (bee-log-filename log-dir bee-name)
  (simple-format #f "~A/~A.log" log-dir bee-name))

(define-public *service-data-directory* "/var/lib/swarm/")

(define-public (swarm-data-directory swarm-name)
  (string-append *service-data-directory* swarm-name))

(define-public (bee-data-directory swarm-name bee-name)
  (simple-format #f "~A/~A" (swarm-data-directory swarm-name) bee-name))

(define-public (bee-wallet-file swarm-name bee-name)
  (simple-format #f "~A/keys/swarm.key" (bee-data-directory swarm-name bee-name)))

(define-public (bee-password-file swarm-name bee-name)
  (simple-format #f "~A/~A.password" (swarm-data-directory swarm-name) bee-name))

(define-public* (wallet-file-address filename)
  (let* ((json (call-with-input-file filename
                 json->scm))
         (address (assoc-ref json "address")))
    address))
