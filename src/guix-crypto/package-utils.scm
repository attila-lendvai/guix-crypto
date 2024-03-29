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

(define-module (guix-crypto package-utils)
  #:use-module (guix-crypto utils)
  #:use-module (guix build utils)
  #:use-module (guix diagnostics)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:export (read-hashes-file))

(define (%read-module-relative-file module filename)
  (with-input-from-file
      (or (search-path %load-path
                       (string-append (dirname (module-filename module))
                                      "/" filename))
          (error "%read-module-relative-file failed for" filename))
    (lambda _
      (values (read)     ; version
              (read))))) ; hashes

(define-syntax read-hashes-file
  (lambda (syn)
    (syntax-case syn ()
      ((_ filename)
       (with-syntax
           ;; Reads the file at compile time and macroexpands to the first form in it.
           ((form (call-with-values
                      (lambda _
                        (%read-module-relative-file (current-module)
                                                    (string-append "hashes/"
                                                                   (syntax->datum #'filename)
                                                                   ".hashes")))
                    (lambda (version hashes)
                      #`(values '#,version '#,hashes)))))
         #'form)))))

(define-public (unsupported-arch package-name system)
  (raise (formatted-message
          (G_ "The package '~a' does not support the Guix system '~a'")
          package-name system)))

(define-public (guix-system-name->rust-system-name name)
  (match name
    ("x86_64-linux"      "x86_64-unknown-linux")
    ("aarch64-linux"     "aarch64-unknown-linux")))

(define-public (guix-system-name->go-system-name system)
  (match system
    ("x86_64-linux"      "linux-amd64")
    ("i686-linux"        "linux-386")
    ("aarch64-linux"     "linux-arm64")
    ;; some of the rest may be wrong
    ("armhf-linux"       "linux-arm")
    ("mips64el-linux"    "linux-mips64le")
    ("powerpc64le-linux" "linux-ppc64le")
    ("powerpc-linux"     "linux-ppc")
    ("riscv64-linux"     "linux-riscv64")))

(define-public (current-system-as-go-system)
  (guix-system-name->go-system-name (%current-system)))

(define-public (current-system-as-rust-system)
  (guix-system-name->rust-system-name (%current-system)))

(define-public* (github-release-uri org-name repo-name version file-name
                                    #:key (suffix ""))
  (values (string-append
           "https://github.com/" org-name "/" repo-name "/releases/download/"
           "v" version "/" file-name suffix)
          file-name))

;;;
;;; Geth
;;;
(define-public* (geth-release-file-name arch version commit-hash
                                        #:key (suffix ""))
  (string-append "geth-alltools-"
                 arch
                 "-"
                 version "-"
                 commit-hash
                 ".tar.gz"
                 suffix))

(define-public* (geth-release-uri arch version commit-hash
                                  #:key (suffix ""))
  (let ((file-name (geth-release-file-name arch version commit-hash
                                           #:suffix suffix)))
    (values (string-append "https://gethstore.blob.core.windows.net/builds/"
                           file-name)
            file-name)))

;;;
;;; Swarm Bee
;;;
(define-public* (bee-release-file-name arch)
  (string-append "bee-" arch))

(define-public* (bee-release-uri arch version)
  (github-release-uri "ethersphere" "bee" version
                      (bee-release-file-name arch)))

;;;
;;; Swarm Tools
;;;

;; https://github.com/rndlabs/swarm-tools-rs/releases/download/v0.3.2/swarm-tools-rs_v0.3.2_x86_64-unknown-linux-musl.tar.gz
(define-public* (swarm-tools/release-file-name arch version)
  (string-append "swarm-tools-rs_v" version "_" arch "-musl.tar.gz"))

(define-public* (swarm-tools/release-uri arch version)
  (github-release-uri "rndlabs" "swarm-tools-rs" version
                      (swarm-tools/release-file-name arch version)))

;;;
;;; Lighthouse
;;;
(define-public* (lighthouse-release-file-name arch version)
  (string-append "lighthouse-v" version "-" arch
                 "-gnu-portable.tar.gz"))

(define-public* (lighthouse-release-uri arch version
                                        #:key (suffix ""))
  (github-release-uri "sigp" "lighthouse" version
                      (lighthouse-release-file-name arch version)
                      #:suffix suffix))

;;;
;;; Feather wallet
;;;

;; https://featherwallet.org/files/releases/linux/feather-2.3.0-linux.zip
(define-public* (feather-release-file-name version #:key (suffix ""))
  (string-append "feather-" version "-linux.zip"
                 suffix))

(define-public* (feather-release-uri arch version #:key (suffix ""))
  (let ((file-name (feather-release-file-name version #:suffix suffix)))
    (values (string-append "https://featherwallet.org/files/releases/linux/"
                           file-name)
            file-name)))

;;;
;;; ZCash
;;;
(define-public (guix-system-name->zcash-system-name name)
  (match name
    ("x86_64-linux"      "linux64")))

(define-public* (zcash-release-file-name arch version
                                         #:key (suffix ""))
  (string-append "zcash-"
                 version "-"
                 arch
                 "-debian-bullseye.tar.gz"
                 suffix))

(define-public* (zcash-release-uri arch version #:key (suffix ""))
  (let ((file-name (zcash-release-file-name arch version #:suffix suffix)))
    (values (string-append "https://z.cash/downloads/" file-name)
            file-name)))

;;;
;;; Nethermind
;;;
(define-public (guix-system-name->nethermind-system-name name)
  (match name
    ("x86_64-linux"      "linux-x64")
    ("aarch64-linux"     "linux-arm64")))

(define-public* (nethermind-release-file-name arch version commit
                                              #:key (suffix ""))
  (string-append "nethermind-"
                 version "-"
                 commit "-"
                 arch
                 ".zip"
                 suffix))

(define-public* (nethermind-release-uri arch version commit #:key (suffix ""))
  (let ((file-name (nethermind-release-file-name arch version commit #:suffix suffix)))
    (values (string-append "https://nethdev.blob.core.windows.net/builds/"
                           file-name)
            file-name)))
