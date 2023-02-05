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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-71))

(define-public zcash-binary
  ;; Note: use bin/geth-update-helper.scm to update the hashes
  (let ((hashes (read-hashes-file "zcash-binary")))
    (package
      (name "zcash-binary")
      (version "4.7.0")
      (source
       (let* ((uri file-name
                   (zcash-release-uri (guix-system-name->zcash-system-name (%current-system))
                                      version)))
         (origin
           (method url-fetch)
           (uri uri)
           (file-name file-name)
           (sha256 (base32 (or (assoc-ref hashes (%current-system))
                               (unsupported-arch name (%current-system))))))))
      (build-system binary-build-system)
      (arguments
       (list
        #:imported-modules (source-module-closure
                            `((guix-crypto utils)
                              ,@%binary-build-system-modules)
                            #:select? default-module-filter)
        #:modules '((guix build utils)
                    (guix-crypto utils)
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
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "tar" "--strip-components=1" "-xzvf"
                        source)))
            (add-after 'patchelf 'check
              (lambda* (#:key (tests? #t) #:allow-other-keys)
                (when tests?
                  ;; At the time of this writing binary-build-system does not
                  ;; support cross builds. When it will, it will hopefully
                  ;; declare #:tests #f and this will keep working in cross
                  ;; builds.
                  (invoke "./bin/zcash-cli" "--version")
                  (invoke "./bin/zcashd" "--version")))))))
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
      (license license:expat)
      (properties
       '((release-monitoring-url . "https://github.com/NethermindEth/nethermind/releases"))))))
