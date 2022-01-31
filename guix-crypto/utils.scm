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
  #:use-module (guix diagnostics)
  #:use-module (guix ui)
  #:use-module (ice-9 match))

(define-public (unsupported-arch package-name system)
  (raise (formatted-message
          (G_ "The package '~a' does not support the Guix system '~a'")
          package-name system)))

(define-public (guix-system-name->go-system-name package-name system)
  (match system
    ("x86_64-linux"      "linux-amd64")
    ("i686-linux"        "linux-386")
    ("aarch64-linux"     "linux-arm64")
    (_ (unsupported-arch package-name system))))

(define-public (github-download-link org-name repo-name version file-name)
  (string-append
   "https://github.com/" org-name "/" repo-name "/releases/download/"
   "v" version "/" file-name))
