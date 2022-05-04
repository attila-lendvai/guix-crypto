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
;;; This file does not bring in a large transitive closure of
;;; dependencies (i.e. no dependency on GEXP stuff), and thus can be
;;; used in the Shephard sice of the code (e.g. in the service start
;;; forms).
;;;

(define-module (guix-crypto script-utils)
  #:use-module (guix build utils)
  ;;#:use-module (guix openpgp)
  ;; TODO delme #:use-module (guix build utils)
  ;; delme #:use-module (guix build syscalls)
  #:use-module ((scheme base) #:select (call-with-port))
  #:use-module (system repl error-handling) ; from Guile
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:export        ; Also note the extensive use of DEFINE-PUBLIC below
  (with-log-directory
   match-record ;; TODO temporarily, see below
   with-service-environment
   define-public*))

(define-public *tmp-directory* (make-parameter 'tmp-dir-not-initialized))

(define-public (tmp-file . name-parts)
  (apply string-append (*tmp-directory*) "/" name-parts))

(define-public (verify-gpg-signature key-fingerprints key-files
                                     signature-file data-file)
  (let ((gpg-homedir (mkdtemp "/tmp/gpg-homedir.XXXXXX")))
    (dynamic-wind
      (const #t)
      (lambda ()
        (let* ((gpg-options (list "gpg"
                                  "--homedir" gpg-homedir
                                  "--no-options"
                                  "--trust-model" "tofu"
                                  "--no-auto-check-trustdb"
                                  "--no-default-keyring"))
               (invoke-gpg (lambda args
                             (apply invoke (append gpg-options args)))))
          (mkdir-p gpg-homedir)
          (chmod gpg-homedir #o700)
          ;; import the keys
          (for-each
           (cut invoke-gpg "--import" <>)
           key-files)
          ;; trust them
          (for-each
           (cut invoke-gpg "--tofu-policy" "good" <>)
           key-fingerprints)
          ;; verify the archive
          (invoke-gpg "--verify" signature-file data-file)
          (values #true)))
      (lambda ()
        (delete-file-recursively gpg-homedir)))))

;; This is a WIP of a scheme-only way to verify the signatures.
;; Not sure it's a good idea, though. Another codebase to trust...
;; (define (foo)
;;   (let* ((sig-asc (read-file-to-string "/tmp/x/nethermind-linux-amd64-1.12.8-2d3dd48.zip.asc"))
;;          (sig (string->openpgp-packet sig-asc)))
;;     (with-input-from-file "/tmp/x/nethermind-linux-amd64-1.12.8-2d3dd48.zip"
;;       (lambda (data-port)
;;         (verify-openpgp-signature sig ))
;;       #:binary #true)))
