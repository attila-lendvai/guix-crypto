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

(define-module (guix-crypto packages ethereum)
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto package-utils)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix modules)
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

;; As per https://geth.ethereum.org/downloads/#openpgp_signatures
;; captured at 2021-10-29.
(define +geth-key-fingerprint+
  "FDE5A1A044FA13D2F7ADA019A61A13569BA28146")

(define +geth-url-base+ "https://gethstore.blob.core.windows.net/builds/")

(define* (geth-release-file-name version commit-hash #:optional (suffix ""))
  (string-append "geth-alltools-"
                 (guix-system-name->go-system-name "geth-binary"
                                                   (%current-system))
                 "-"
                 version "-"
                 commit-hash
                 ".tar.gz"
                 suffix))

(define* (geth-release-origin version hash commit-hash #:optional (suffix ""))
  (let ((file-name (geth-release-file-name version commit-hash suffix)))
    (origin
      (method url-fetch)
      (uri (string-append +geth-url-base+ file-name))
      (file-name file-name)
      (sha256 hash))))

(define-public geth-binary
  (let* ((version "1.10.17")
         (commit-hash "25c9b49f") ; first 8 digits of the tagged commit's hash
         (signing-key (local-file "geth-signing-key.asc"))
         (signature (geth-release-origin
                     version
                     (match (%current-system)
                       ("i686-linux"
                        (base32 "047ya79k4xlw4d1bgyjyslh0vv3j7g0p3b5hg1221k53gs2zgpx4"))
                       ("x86_64-linux"
                        (base32 "17m4gw6p93c25mdvw2zvmgbk7bacqvai73gmn4z9qc6zbpvp111h"))
                       ("aarch64-linux"
                        (base32 "13gkb8m8hspjxi91ai9agsbqq2d0p5yl166vjqdwhd8f7zzsj6fc"))
                       (_ (unsupported-arch name (%current-system))))
                     commit-hash
                     ".asc")))
    (package
      (name "geth-binary")
      (version version)
      (source
       (geth-release-origin
        version
        (match (%current-system)
          ;; To update the hashes go to https://geth.ethereum.org/downloads/
          ;; then download the alltools files for the archs, and then run
          ;; guix hash geth-linux-amd64-1.10.15-8be800ff.tar.gz
          ("i686-linux"
           (base32 "05pbyc2wwqla262r09iwv506mfwih31i7ln5zyiy82hkvbdv8d4n"))
          ("x86_64-linux"
           (base32 "1kljbr3ks2dn6jd87k7l0xaasbk82rrxmaxjkm2vy7cvaxwaq0cw"))
          ("aarch64-linux"
           (base32 "19100yqrd7z8f9cga4a52hygv93wn3syhi7ix4hi9km34v1qi89d"))
          (_ (unsupported-arch name (%current-system))))
        commit-hash))
      (outputs '("out" "clef" "evm"))
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
        #:strip-binaries? #f            ; The less we modify, the better.
        #:patchelf-plan ''(("geth")
                           ("clef")
                           ("evm"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'unpack 'check-signatures
              (lambda* (#:key source #:allow-other-keys)
                (verify-gpg-signature #$+geth-key-fingerprint+
                                      #$signing-key
                                      #$signature
                                      source)))
            (replace 'unpack
              (lambda* (#:key source native-inputs #:allow-other-keys)
                (invoke "tar" "--strip-components=1" "-xzvf" source)))
            (replace 'install
              ;; #:install-plan doesn't seem to be capable of producing
              ;; multiple outputs.
              (lambda* (#:key system outputs #:allow-other-keys)
                (let ((out  (assoc-ref outputs "out"))
                      (clef (assoc-ref outputs "clef"))
                      (evm  (assoc-ref outputs "evm"))
                      (doit (lambda (name target)
                              (let ((target-dir (string-append target "/bin")))
                                (mkdir-p target-dir)
                                (copy-file name (string-append target-dir "/" name))))))
                  (doit "geth" out)
                  (doit "clef" clef)
                  (doit "evm"  evm)))))))
      (native-inputs
       (list gnupg
             patchelf))
      (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"))
      (home-page "https://geth.ethereum.org/")
      (synopsis "Official Go implementation of the Ethereum protocol")
      (description
       "Ethereum is a decentralized platform that runs smart contracts,
applications that run exactly as programmed without possibility of downtime,
censorship, fraud or third party interference.")
      (license license:gpl3+))))

(define-public openethereum-binary
  (let ((version "3.3.5"))
    (package
      (name "openethereum-binary")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (github-download-link "openethereum" "openethereum" version
                                    (string-append "openethereum-linux-v"
                                                   version ".zip")))
         (sha256
          (match (%current-system)
            ("x86_64-linux"
             (base32
              "0sncd0r5pg0gayapfb6irpqh252ma7z36cnd9ahzg5nl6a5c8wmd"))
            (_ (unsupported-arch name (%current-system)))))))
      (build-system binary-build-system)
      (arguments
       `(#:install-plan `(("openethereum" "bin/"))
         #:strip-binaries? #false          ; The less we modify, the better.
         #:patchelf-plan `(("openethereum" ("gcc" "glibc")))))
      (native-inputs (list unzip patchelf))
      (inputs (list (list gcc "lib") glibc))
      (supported-systems '("x86_64-linux"))
      (home-page "https://openethereum.org/")
      (synopsis "Fast and feature-rich Ethereum client")
      (description
       "OpenEthereum’s goal is to be the fastest, lightest, and most secure
Ethereum client.  We are developing OpenEthereum using the cutting-edge Rust
programming language.")
      (license license:gpl3+))))
