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

(define-module (guix-crypto swarm-utils)
  #:use-module (guix-crypto utils)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 format)
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

(define-public* (spawn-bee binary config-file action swarm-name bee-index user group
                           #:key
                           resource-limits
                           resolver-options
                           blockchain-rpc-endpoint
                           eth-address)
  (log.dribble "About to spawn bee action ~S, with config ~S, and binary ~S" action config-file binary)
  (let ((data-dir (bee-data-directory swarm-name bee-index)))
    ;; TODO use spawn instead
    (apply (@@ (shepherd) fork+exec-command) ; TODO why can't i just #:use-module (shepherd) above?
           (list binary
                 "--config" config-file
                 action)
           #:user user
           #:group group
           #:log-file (bee-log-filename (default-log-directory swarm-name)
                                        bee-index)
           #:directory data-dir
           #:environment-variables
           (remove unspecified?
                   (list
                    "LC_ALL=en_US.UTF-8"
                    (string-append "HOME=" data-dir)
                    ;; So that these are not visible with ps, or in the
                    ;; config file (i.e. world-readable under
                    ;; /gnu/store/), because they may contain keys when
                    ;; using a service like Infura.
                    (when blockchain-rpc-endpoint
                      (string-append "BEE_BLOCKCHAIN_RPC_ENDPOINT="
                                     blockchain-rpc-endpoint))
                    (when resolver-options
                      (string-append "BEE_RESOLVER_OPTIONS=" resolver-options))
                    (when eth-address
                      (string-append "BEE_CLEF_SIGNER_ETHEREUM_ADDRESS=" eth-address))))

           (if resource-limits
               (list #:resource-limits resource-limits)
               '()))))
