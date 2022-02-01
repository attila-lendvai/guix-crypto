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
  #:use-module (guix-crypto utils)
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
(define-public (default-log-directory swarm-name)
  (string-append "/var/log/swarm/" swarm-name))

(define-public (bee-log-file swarm-name bee-index)
  (string-append (*log-directory*)
                 "/bee-" (number->string bee-index) ".log"))

(define-public (clef-log-file swarm-name)
  (string-append (*log-directory*) "/clef.log"))

;;;
;;; Data dir
;;;
(define +service-data-directory+ "/var/lib/swarm/")

(define-public (swarm-data-directory swarm-name)
  (string-append +service-data-directory+ swarm-name))

(define-public (bee-data-directory swarm-name bee-index)
  (string-append (swarm-data-directory swarm-name) "/bee-"
                 (number->string bee-index)))

(define-public (bee-account-file swarm-name bee-index)
  (string-append (bee-data-directory swarm-name bee-index) "/eth-address"))

(define-public (clef-data-directory swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef"))

(define-public (clef-ipc-file swarm-name)
  (string-append (clef-data-directory swarm-name) "/clef.ipc"))

(define-public (clef-keystore-directory swarm-name)
  (string-append (clef-data-directory swarm-name) "/keystore"))

(define-public (bee-password-file swarm-name)
  (string-append (swarm-data-directory swarm-name) "/bee-password"))

(define-public (clef-password-file swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef-password"))

(define-public (clef-service-name swarm-name)
  (string->symbol (simple-format #f "clef-~A" swarm-name)))

(define-public (bee-service-name swarm-name bee-index)
  (string->symbol (simple-format #f "bee-~A-~A" swarm-name bee-index)))
