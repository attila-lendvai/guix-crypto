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

;; As per https://geth.ethereum.org/downloads/#openpgp_signatures
;; captured at 2021-10-29.
(define +geth-linux-builder-key-fingerprint+
  "FDE5A1A044FA13D2F7ADA019A61A13569BA28146")

(define +geth-linux-builder-key+
  "-----BEGIN PGP PUBLIC KEY BLOCK-----
Comment: Hostname:
Version: Hockeypuck ~unreleased

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
aD1XJM7Aqq5KSSLvJMap9uHNO3WQXuNOoeLsSg==
=1D3S
-----END PGP PUBLIC KEY BLOCK-----")

(define +geth-url-base+ "https://gethstore.blob.core.windows.net/builds/")

(define (unsupported-arch package-name system)
  (raise (formatted-message
          (G_ "The package '~a' does not support the Guix system '~a'")
          package-name system)))

(define (guix-system-name->go-system-name package-name system)
  (match system
    ("x86_64-linux"      "linux-amd64")
    ("i686-linux"        "linux-386")
    ("aarch64-linux"     "linux-arm64")
    (_ (unsupported-arch package-name system))))

(define* (github-download-link org-name repo-name version file-name)
  (string-append
   "https://github.com/" org-name "/" repo-name "/releases/download/"
   "v" version "/" file-name))

(define (geth-directory system version commit-hash)
  (string-append "geth-alltools-"
                 (guix-system-name->go-system-name "geth-binary" system) "-"
                 version "-"
                 commit-hash))

(define-public geth-binary
  (let ((version "1.10.13")
        (commit-hash "7a0c19f8"))
    (package
      (name "geth-binary")
      (version version)
      (source #false)                   ; see below
      (outputs '("out" "clef" "evm"))
      (build-system binary-build-system)
      (arguments
       `(#:modules ((nonguix build binary-build-system)
                    (guix build utils))
         #:strip-binaries? #f          ; The less we modify, the better.
         #:patchelf-plan
         '(("geth")
           ("clef")
           ("evm"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'unpack 'check-signatures
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((gpg-homedir "gpg-homedir")
                      (key-file "geth-linux-builder.asc")
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
                 (with-output-to-file key-file
                   (lambda _
                     (display ,+geth-linux-builder-key+)))
                 ;; import the key
                 (invoke-gpg "--import" key-file)
                 ;; trust it
                 (invoke-gpg "--tofu-policy" "good"
                             ,+geth-linux-builder-key-fingerprint+)
                 ;; verify the archive
                 (invoke-gpg "--verify"
                             (assoc-ref inputs "signature")
                             (assoc-ref inputs "source")))
               #t))
           (replace 'unpack
             (lambda* (#:key inputs #:allow-other-keys)
               (invoke "tar" "--strip-components=1" "-xzvf"
                       (assoc-ref inputs "source"))
               #t))
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
                 (doit "evm"  evm))
               #t)))))
      (native-inputs
       (list gnupg patchelf))
      (inputs
       `(("source"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   +geth-url-base+
                   (geth-directory (%current-system) version commit-hash)
                   ".tar.gz"))
             (sha256
              (match (%current-system)
                ("x86_64-linux"
                 (base32
                  "0v7bc6gmwz7dky5dcmkivnkiwvm3c6fziw5z4kr62pn5mc86pawk"))
                ("i686-linux"
                 (base32
                  "1wlx782r3hjb23dydrwzzwg37mhywhkgrpirqw6kmlzpk8fc5alz"))
                ("aarch64-linux"
                 (base32
                  "1wi4fgj7r1vgffdrxy2yp8c28ph8z21c9q9shipav10606rjrici"))
                (_ (unsupported-arch name (%current-system)))))))
         ("signature"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   +geth-url-base+
                   (geth-directory (%current-system) version commit-hash)
                   ".tar.gz.asc"))
             (sha256
              (match (%current-system)
                ("x86_64-linux"
                 (base32
                  "0gp94l4s0m21ldp7i7n6x73cmpq2m57a24pq5lkn5v8a6csn2m8d"))
                ("i686-linux"
                 (base32
                  "160vxwxk9jhjip8lhacxbgiswg17brbcl7h0fdljjgn7smcmn0gb"))
                ("aarch64-linux"
                 (base32
                  "0knlk1gbfyzjlw4dnd91y9sysqsgvsxwzl33cvn3sbr0frirdg1a"))
                (_ (unsupported-arch name (%current-system)))))))))

      (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"))

      (home-page "https://geth.ethereum.org/")
      (synopsis "Official Go implementation of the Ethereum protocol")
      (description
       "Ethereum is a decentralized platform that runs smart contracts,
applications that run exactly as programmed without possibility of downtime,
censorship, fraud or third party interference.")
      (license license:gpl3+))))
