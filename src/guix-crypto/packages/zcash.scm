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

(define-module (guix-crypto packages zcash)
  #:use-module (guix-crypto package-utils)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match))

(define-public zcash-binary
  (let ((version "4.7.0"))
    (package
      (name "zcash-binary")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (simple-format #f "https://z.cash/downloads/zcash-~A-linux64-debian-bullseye.tar.gz"
                             version))
         (sha256
          (match (%current-system)
            ("x86_64-linux"
             (base32
              "1c6hfli4wbdw2im51ak1yfg59xnsv33qsiilr24nygbxdp6p1awm"))
            (_ (unsupported-arch name (%current-system)))))))
      (build-system binary-build-system)
      (arguments
       `(#:install-plan `(("bin/zcash-cli"          "bin/")
                          ("bin/zcashd"             "bin/")
                          ("bin/zcashd-wallet-tool" "bin/")
                          ("bin/zcash-fetch-params" "bin/")
                          ("bin/zcash-tx"           "bin/")
                          ("share/" "share/"))
         #:strip-binaries? #false       ; The less we modify, the better.
         #:patchelf-plan `(("bin/zcash-cli"          ("gcc" "glibc"))
                           ("bin/zcashd"             ("gcc" "glibc"))
                           ("bin/zcashd-wallet-tool" ("gcc" "glibc"))
                           ("bin/zcash-tx"           ("gcc" "glibc")))))
      (native-inputs (list patchelf))
      (inputs (list (list gcc "lib") glibc))
      (supported-systems '("x86_64-linux"))
      (home-page "https://z.cash")
      (synopsis "Zcash blockchain tools")
      (description "The official Zcash binary release patched to work on Guix.")
      (license license:expat))))
