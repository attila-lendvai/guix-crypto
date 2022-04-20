;;; Copyright © 2022 Attila Lendvai <attila@lendvai.name>
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

(define-module (guix-crypto build-utils)
  #:use-module (guix build utils)
  #:use-module (guix build syscalls))

(define-public (verify-gpg-signature key-fingerprint key-file signature blob)
  (let* ((gpg-homedir (mkdtemp! "gpg-homedir.XXXXXX"))
         (gpg-options (list "gpg"
                            "--homedir" gpg-homedir
                            "--no-options"
                            "--trust-model" "tofu"
                            "--no-auto-check-trustdb"
                            "--no-default-keyring"))
         (invoke-gpg (lambda args
                       (apply invoke (append gpg-options args)))))
    (mkdir-p gpg-homedir)
    (chmod gpg-homedir #o700)
    ;; import the key
    (invoke-gpg "--import" key-file)
    ;; trust it
    (invoke-gpg "--tofu-policy" "good" key-fingerprint)
    ;; verify the archive
    (invoke-gpg "--verify" signature blob)))
