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
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages tls)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71))

(define-public geth-binary
  (let ((commit-hash "7371b381")  ; first 8 digits of the tagged commit's hash
        (version hashes (read-hashes-file "geth-binary")))
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
        #:strip-binaries? #f            ; The less we modify, the better.
        #:patchelf-plan ''(("geth")
                           ("clef")
                           ("evm"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
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
      (license license:gpl3+)
      (properties
       '((release-monitoring-url . "https://github.com/ethereum/go-ethereum/releases"))))))

(define-public nethermind-binary
  (let ((commit-hash "e8c161a5")       ; first 8 digits of the tagged commit's hash
        (version hashes (read-hashes-file "nethermind-binary")))
    (package
      (name "nethermind-binary")
      (version version)
      (source
       (let* ((uri file-name
                   (nethermind-release-uri
                    ;; Strictly speaking, this is wrong, because it's
                    ;; not written in golang, but it matches.
                    (guix-system-name->nethermind-system-name (%current-system))
                    version commit-hash)))
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
          ;; We install the binaries into the share-dir, so that they can find
          ;; the necessary files relative to the binary's path. They are
          ;; symlinked into the bin/ dir in a separate phase below.
          #:install-plan `'(("Nethermind.Cli"      ,share-dir)
                            ("Nethermind.Launcher" ,share-dir)
                            ("Nethermind.Runner"   ,share-dir)
                            ("NLog.config"         ,share-dir)
                            ("plugins"             ,share-dir)
                            ("configs"             ,share-dir))
          #:strip-binaries? #false      ; The less we modify, the better.
          #:patchelf-plan (let ((libs '("glibc" "gcc" "zlib" "icu4c"
                                        "rocksdb" "openssl" "snappy")))
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
                ;; the NETHERMIND_PLUGINSDIRECTORY var doesn't seem to work
                ;; if it worked, we could put the binaries into bin/ and wrap them.
                ;; not sure whether it would be any better than
                ;; symlinking them, though...
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
              ;; alternatively:
              ;; (add-after 'install 'wrap
              ;;   (lambda* (#:key outputs #:allow-other-keys)
              ;;     (let ((out (assoc-ref outputs "out")))
              ;;       (wrap-program (string-append out "/bin/Nethermind.Runner")
              ;;         `("NETHERMIND_PLUGINSDIRECTORY" =
              ;;           (,(string-append out #$share-dir "plugins")))))))
              ))))
      (native-inputs (list unzip
                           patchelf))
      (inputs (list (list gcc "lib")
                    glibc
                    icu4c
                    openssl
                    rocksdb
                    snappy
                    zlib))
      (supported-systems (map first hashes))
      (home-page "https://nethermind.io/")
      (synopsis "Ethereum client based on .NET Core")
      (description "The official Nethermind binary release, patched to
work on Guix.")
      (license license:gpl3+)
      (properties
       '((release-monitoring-url . "https://github.com/NethermindEth/nethermind/releases"))))))

(define-public lighthouse-binary
  (let ((version hashes (read-hashes-file "lighthouse-binary")))
    (package
      (name "lighthouse-binary")
      (version version)
      (source
       (let* ((uri file-name
                   (lighthouse-release-uri
                    (guix-system-name->rust-system-name (%current-system))
                    version)))
         (origin
           (method url-fetch)
           (uri uri)
           (file-name file-name)
           (sha256 (base32 (or (assoc-ref hashes (%current-system))
                               (unsupported-arch name (%current-system))))))))
      (build-system binary-build-system)
      (arguments
       (let ((versioned-name (string-append "lighthouse-" version)))
         (list
          #:install-plan `'(("lighthouse" ,(string-append "bin/" versioned-name)))
          #:strip-binaries? #false      ; The less we modify, the better.
          #:patchelf-plan (let ((libs '("glibc" "gcc")))
                            `'(("lighthouse" ,libs)))
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'patchelf 'check
                (lambda* (#:key (tests? #t) #:allow-other-keys)
                  (when tests?
                    ;; At the time of this writing binary-build-system does not
                    ;; support cross builds. When it will, it will hopefully
                    ;; declare #:tests #f and this will keep working in cross
                    ;; builds.
                    (invoke "./lighthouse" "--version"))))
              (add-after 'install 'symlink-binaries
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (source #$versioned-name)
                         (target "lighthouse"))
                    (with-directory-excursion (string-append out "/bin/")
                      (format #t "~A -> ~A~%" source target)
                      (symlink source target)))))))))
      (native-inputs (list unzip
                           patchelf))
      (inputs (list (list gcc "lib")
                    glibc))
      (supported-systems (map first hashes))
      (home-page "https://lighthouse.sigmaprime.io/")
      (synopsis "Ethereum consensus-layer client, aka a beacon node")
      (description "The official lighthouse binary release, patched to
work on Guix.")
      (license license:asl2.0)
      (properties
       '((release-monitoring-url . "https://github.com/sigp/lighthouse/releases"))))))

(define-public nimbus-binary
  (let ((commit-hash "d014d0a5")  ; first 8 digits of the tagged commit's hash
        (version hashes (read-hashes-file "nimbus-binary")))
    (package
      (name "nimbus-binary")
      (version version)
      (source
       (let* ((uri file-name
                   (nimbus-release-uri
                    (guix-system-name->nimbus-system-name (%current-system))
                    version commit-hash)))
         (origin
           (method url-fetch)
           (uri uri)
           (file-name file-name)
           (sha256 (base32 (or (assoc-ref hashes (%current-system))
                               (unsupported-arch name (%current-system))))))))
      (build-system binary-build-system)
      (arguments
       (let* ((binaries '("nimbus_beacon_node"
                          "nimbus_validator_client"))
              (versioned-names (map (lambda (binary)
                                      (string-append binary "-" version))
                                    binaries)))
         (list
          #:install-plan `'(,@(map (lambda (name versioned-name)
                                     (list (string-append "build/" name)
                                           (string-append "bin/"   versioned-name)))
                                   binaries versioned-names))
          #:strip-binaries? #false      ; The less we modify, the better.
          ;; TODO investigate: without the patchelf for glibc the binary seems
          ;; to run fine (check phase finishes), but then after that the
          ;; runpath validation phase fails?!
          #:patchelf-plan (let ((libs '("glibc")))
                            `'(,@(map (lambda (binary)
                                        (list (string-append "build/" binary)
                                              libs))
                                      binaries)))
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'patchelf 'check
                (lambda* (#:key (tests? #t) #:allow-other-keys)
                  (when tests?
                    ;; At the time of this writing binary-build-system does not
                    ;; support cross builds. When it will, it will hopefully
                    ;; declare #:tests #f and this will keep working in cross
                    ;; builds.
                    (map (lambda (binary)
                           (invoke (string-append "./build/" binary) "--version"))
                         '#$binaries))))
              (add-after 'install 'symlink-binaries
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out")))
                    (with-directory-excursion (string-append out "/bin/")
                      (map
                       (lambda (binary versioned-name)
                         (let ((source versioned-name)
                               (target binary))
                           (format #t "~A -> ~A~%" source target)
                           (symlink source target)))
                       '#$binaries '#$versioned-names)))))))))
      (native-inputs (list tar
                           gzip
                           patchelf))
      (inputs (list (list gcc "lib")
                    glibc))
      (supported-systems (map first hashes))
      (home-page "https://nimbus.guide/")
      (synopsis "Ethereum consensus-layer client, aka a beacon node")
      (description "The official Nimbus binary release, patched to work on Guix.")
      (license license:asl2.0)
      (properties
       '((release-monitoring-url . "https://github.com/status-im/nimbus-eth2/releases/latest"))))))
