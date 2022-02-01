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

(define-module (guix-crypto packages swarm)
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

(define-public bee-binary
  (let ((version "1.4.1"))
    (package
      (name "bee-binary")
      (version version)
      (source #false) ; see below
      (build-system binary-build-system)
      (arguments
       `(#:install-plan `(("bee" "bin/"))
         #:strip-binaries? #false          ; The less we modify, the better.
         #:phases
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-file (assoc-ref inputs "source") "bee")
               (chmod "bee" #o555)
               #true)))))
      (inputs
       `(("source"
          ,(origin
             (method url-fetch)
             (uri (github-download-link
                   "ethersphere" "bee" version
                   (string-append "bee-"
                                  (guix-system-name->go-system-name
                                   name (%current-system)))))
             (sha256
              ;; To update these go to https://github.com/ethersphere/bee/releases/
              ;; then download the relevant files, and then `guix hash` them.
              (match (%current-system)
                ("x86_64-linux"
                 (base32
                  "0lhnq97ql2xisrzhwysygf2skvw43vhpy8cwn5xv5isrmjsdfqsm"))
                ("i686-linux"
                 (base32
                  "0ap9aivv7lzqng5z3zl5j5x0qxph5zpirz0afwkb3ikwn9g4p6n2"))
                ("aarch64-linux"
                 (base32
                  "1rl5msprdwapjs4gfx83f58dhj0zs1k5av8q4d9mrf7i7gi85sdw"))
                (_ (unsupported-arch name (%current-system)))))))))

      (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"))

      (home-page "https://www.ethswarm.org/")
      (synopsis "Bee is a Swarm client implemented in Go")
      (description
       "Swarm is a system of peer-to-peer networked nodes that create a decentralised
storage and communication service.  The system is economically self-sustaining due to
a built-in incentive system enforced through smart contracts on the Ethereum
blockchain.  Bee is a Go implementation of a node in a Swarm.")
      (license license:bsd-3))))
