#!/usr/bin/env -S guile --no-auto-compile -e '(@@ (guix-crypto bee-release-hashes) main)' -s
!#

;;; Helper script to update the package definition at new releases.

;; TODO this is currently broken. seems like http-fetch breaks on the redirects.

(define-module (guix-crypto bee-release-hashes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-60)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (rnrs io ports)

  #:use-module (gcrypt hash)
  #:use-module (guix http-client)
  #:use-module (guix base32))

(define (url-hash url)
  (call-with-port
      (http-fetch url)
    (cut port-hash (hash-algorithm sha256) <>)))

(define* (bee-url arch version #:optional (suffix ""))
  (format #f "https://github.com/ethersphere/bee/releases/download/v~A/bee-linux-~A~A"
          version arch suffix))

(define* (print-hash arch version #:optional (suffix ""))
  (format #t "Fetching URL [~A]~%" (bee-url arch version suffix))
  (format #t "~A\t" arch)
  (let ((hash (url-hash (bee-url arch version suffix))))
    (format #t "~A~%"
            (bytevector->nix-base32-string hash))))

(define (main cmd)
  (if (> (length cmd) 1)
      (let ((archs '("386" "amd64" "arm64"))
            (version (second cmd)))
        (format #t "Fetching the bee binaries; version ~A~%" version)
        (map (cut print-hash <> version) archs))
      (format #t "Usage: ~A version~%" (first cmd))))
