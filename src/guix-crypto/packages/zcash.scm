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
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto package-utils)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix ui)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match))

;; https://apt.z.cash/zcash.asc captured at 2022-04-20.
(define +zcash-key-fingerprint+
  "F9563107CE889AB60386A3B570C830C67EB9DCB4")

(define* (zcash-binary-origin version hash #:optional (suffix ""))
  (let ((file-name (simple-format #f "zcash-~A-linux64-debian-bullseye.tar.gz~A"
                                  version suffix)))
    (origin
     (method url-fetch)
     (uri (string-append "https://z.cash/downloads/" file-name))
     (file-name file-name)
     (sha256 hash))))

(define-public zcash-binary
  (let* ((version "4.7.0")
         (signing-key (local-file "zcash-signing-key.asc"))
         (signature (zcash-binary-origin
                     version
                     (match (%current-system)
                       ("x86_64-linux"
                        (base32 "11mpfgrb5d4jm72mc5xwqlvpml0qd0xcn48bji36cmf47alk6b8x"))
                       (_ (unsupported-arch name (%current-system))))
                     ".asc")))
    (package
      (name "zcash-binary")
      (version version)
      (source (zcash-binary-origin
               version
               (match (%current-system)
                 ("x86_64-linux"
                  (base32 "1c6hfli4wbdw2im51ak1yfg59xnsv33qsiilr24nygbxdp6p1awm"))
                 (_ (unsupported-arch name (%current-system))))))
      (build-system binary-build-system)
      (arguments
       (list
        #:imported-modules (source-module-closure
                            `((guix-crypto build-utils)
                              ,@%binary-build-system-modules)
                            #:select? default-module-filter)
        #:modules '((guix build utils)
                    (guix-crypto build-utils)
                    (nonguix build binary-build-system))
        #:install-plan ''(("bin/zcash-cli"          "bin/")
                          ("bin/zcashd"             "bin/")
                          ("bin/zcashd-wallet-tool" "bin/")
                          ("bin/zcash-fetch-params" "bin/")
                          ("bin/zcash-tx"           "bin/")
                          ("share/" "share/"))
        #:strip-binaries? #false        ; The less we modify, the better.
        #:patchelf-plan ''(("bin/zcash-cli"          ("gcc" "glibc"))
                           ("bin/zcashd"             ("gcc" "glibc"))
                           ("bin/zcashd-wallet-tool" ("gcc" "glibc"))
                           ("bin/zcash-tx"           ("gcc" "glibc")))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'unpack 'check-signatures
              (lambda* (#:key source #:allow-other-keys)
                (verify-gpg-signature #$+zcash-key-fingerprint+
                                      #$signing-key
                                      #$signature
                                      source)))
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "tar" "--strip-components=1" "-xzvf"
                        source))))))
      (native-inputs
       (list gnupg
             patchelf))
      (inputs
       (list (list gcc "lib")
             glibc))
      (supported-systems '("x86_64-linux"))
      (home-page "https://z.cash")
      (synopsis "Zcash blockchain tools")
      (description "The official Zcash binary release, patched to work on Guix.")
      (license license:expat))))
