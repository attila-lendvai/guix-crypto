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
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71))

(define-public geth-binary
  (let* ((commit-hash "23bee162") ; first 8 digits of the tagged commit's hash
         (version "1.10.19")
         ;; Note: use bin/geth-update-helper.scm to update the hashes
         (hashes (read-module-relative-file "geth-binary.hashes")))
    (package
      (name "geth-binary")
      (version version)
      (source
       (let* ((uri file-name
                   (geth-release-uri (current-system-as-go-system)
                                     version commit-hash)))
         (origin
           (method url-fetch)
           (uri uri)
           (file-name file-name)
           (sha256 (base32 (or (assoc-ref hashes (%current-system))
                               (unsupported-arch name (%current-system))))))))
      (outputs '("out" "clef" "evm"))
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
        #:strip-binaries? #f            ; The less we modify, the better.
        #:patchelf-plan ''(("geth")
                           ("clef")
                           ("evm"))
        #:phases
        #~(modify-phases %standard-phases
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
                  (doit "evm"  evm))))
            (add-after 'patchelf 'check
              (lambda* (#:key (tests? #t) #:allow-other-keys)
                (when tests?
                  (invoke "./geth" "version")))))))
      (native-inputs (list gnupg patchelf))
      (inputs (list glibc))
      (supported-systems (map first hashes))
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
         (uri (github-release-uri "openethereum" "openethereum" version
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
       (list
        #:install-plan ''(("openethereum" "bin/"))
        #:strip-binaries? #false          ; The less we modify, the better.
        #:patchelf-plan ''(("openethereum" ("gcc" "glibc")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'patchelf 'check
              (lambda* (#:key (tests? #t) #:allow-other-keys)
                (when tests?
                  ;; At the time of this writing binary-build-system does not
                  ;; support cross builds. When it will, it will hopefully
                  ;; declare #:tests #f and this will keep working in cross
                  ;; builds.
                  (invoke "./openethereum" "--version")))))))
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

(define-public nethermind-binary
  (let* ((version "1.13.2")
         (commit "795dd1e")
         ;; Note: use bin/geth-update-helper.scm to update the hashes
         (hashes (read-module-relative-file "nethermind-binary.hashes")))
    (package
      (name "nethermind-binary")
      (version version)
      (source
       (let* ((uri file-name
                   (nethermind-release-uri
                    ;; Strictly speaking, this is wrong, because it's
                    ;; not written in golang, but it matches.
                    (current-system-as-go-system)
                    version commit)))
         (origin
           (method url-fetch)
           (uri uri)
           (file-name file-name)
           (sha256 (base32 (or (assoc-ref hashes (%current-system))
                               (unsupported-arch name (%current-system))))))))
      (build-system binary-build-system)
      (arguments
       (let ((share-dir (string-append "/share/" name "-" version "/")))
         (list
          #:imported-modules (source-module-closure
                              `((guix-crypto utils)
                                ,@%binary-build-system-modules)
                              #:select? default-module-filter)
          #:modules '((guix build utils)
                      (guix-crypto utils)
                      (nonguix build binary-build-system))
          ;; We install the binaries into the share-dir, so that they can find
          ;; the necessary files relative to the binary's path. They are
          ;; symlinked into the bin/ dir in a separate phase below.
          #:install-plan `'(("Nethermind.Cli"       ,share-dir)
                            ("Nethermind.Launcher"  ,share-dir)
                            ("Nethermind.Runner"    ,share-dir)
                            ("plugins"   ,share-dir)
                            ("configs"   ,share-dir)
                            ("chainspec" ,share-dir))
          #:strip-binaries? #false      ; The less we modify, the better.
          #:patchelf-plan (let ((libs '("glibc" "gcc" "zlib" "icu4c")))
                            `'(("Nethermind.Cli"      ,libs)
                               ("Nethermind.Launcher" ,libs)
                               ("Nethermind.Runner"   ,libs)))
          #:phases
          #~(modify-phases %standard-phases
              (replace 'unpack
                ;; The vanilla unpack arbitrarily enters a subdir.
                ;; See: https://issues.guix.gnu.org/55270
                (lambda* (#:key source #:allow-other-keys)
                  (mkdir "extracted")   ; this is only for aesthetics
                  (chdir "extracted")
                  (invoke "unzip" source)))
              (add-after 'patchelf 'check
                (lambda* (#:key (tests? #t) #:allow-other-keys)
                  (when tests?
                    ;; At the time of this writing binary-build-system does not
                    ;; support cross builds. When it will, it will hopefully
                    ;; declare #:tests #f and this will keep working in cross
                    ;; builds.
                    (setenv "DOTNET_BUNDLE_EXTRACT_BASE_DIR" (getenv "TMPDIR"))
                    (invoke "./Nethermind.Runner" "--version"))))
              (add-after 'install 'symlink-binaries
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (share (string-append out #$share-dir)))
                    (mkdir (string-append out "/bin/"))
                    (for-each
                     (lambda (binary)
                       (let ((source (string-append share binary))
                             (target (string-append out "/bin/" binary)))
                         (format #t "~A -> ~A~%" source target)
                         (symlink source target)))
                     '("Nethermind.Cli"
                       "Nethermind.Launcher"
                       "Nethermind.Runner")))))
              ;; TODO the NETHERMIND_PLUGINSDIRECTORY var doesn't work
              ;; if it worked, we could put the binaries into bin/ and wrap them.
              ;; not sure whether it would be any better, though...
              ;; (add-after 'install 'wrap
              ;;   (lambda* (#:key outputs #:allow-other-keys)
              ;;     (let ((out (assoc-ref outputs "out")))
              ;;       (wrap-program (string-append out "/bin/Nethermind.Runner")
              ;;         `("NETHERMIND_PLUGINSDIRECTORY" =
              ;;           (,(string-append out #$share-dir "plugins")))))))
              ))))
      (native-inputs (list unzip patchelf))
      (inputs (list glibc (list gcc "lib") zlib icu4c))
      (supported-systems (map first hashes))
      (home-page "https://nethermind.io/")
      (synopsis "Ethereum client based on .NET Core")
      (description "The official Nethermind binary release, patched to
work on Guix.")
      (license license:gpl3+))))
