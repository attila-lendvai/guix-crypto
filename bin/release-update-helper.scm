#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

(add-to-load-path (string-append (dirname (current-filename)) "/../src"))

;;; This is a helper script to update the Guix package definition when a new
;;; upstream version is released.
;;;
;;; Example usage:
;;;   ./bin/release-update-helper.scm bee-binary 1.6.0
;;;   ./bin/release-update-helper.scm geth-binary 1.10.17 25c9b49f
;;;   ./bin/release-update-helper.scm nethermind-binary 1.12.8 2d3dd48

(use-modules
 (guix build utils)
 (guix-crypto utils)
 (guix-crypto package-utils)
 (guix-crypto script-utils)

 (srfi srfi-1)
 (srfi srfi-11)
 (srfi srfi-26)
 (srfi srfi-34)
 (srfi srfi-35)
 (srfi srfi-60)
 (ice-9 match)
 (ice-9 rdelim)
 (ice-9 regex)
 (ice-9 format)
 (rnrs io ports)

 (gcrypt hash)
 (guix http-client)
 (guix base32))

(define +package-db+
  `(("geth-binary"
     ,geth-release-uri
     ;; As per https://geth.ethereum.org/downloads/#openpgp_signatures
     ;; captured at 2021-10-29.
     ("FDE5A1A044FA13D2F7ADA019A61A13569BA28146")
     ,guix-system-name->go-system-name
     ("i686-linux" "x86_64-linux" "aarch64-linux"))

    ("bee-binary"
     ,bee-release-uri
     ()                            ; TODO signature is through a checksum file
     ,guix-system-name->go-system-name
     ("i686-linux" "x86_64-linux" "aarch64-linux"))

    ("zcash-binary"
     ,zcash-release-uri
     ;; https://apt.z.cash/zcash.asc captured at 2022-05-05.
     ("3FE63B67F85EA808DE9B880E6DEF3BAF272766C0")
     ,guix-system-name->zcash-system-name
     ("x86_64-linux"))

    ("nethermind-binary"
     ,nethermind-release-uri
     ;; Captured at 2022-05-01.
     ("6942FB745ECE67D86CDA45704770A0C134E353C6"
      "EECCEA1473108E3222D76722D39BE1DDCB6DA407")
     ,guix-system-name->nethermind-system-name
     ("x86_64-linux" "aarch64-linux"))

    ("lighthouse-binary"
     ,lighthouse-release-uri
     ;; Captured at 2022-12-17.
     ("15E66D941F697E28F49381F426416DC3F30674B0")
     ,guix-system-name->rust-system-name
     ("x86_64-linux" "aarch64-linux"))

    ("feather-binary"
     ,feather-release-uri
     ;; Captured at 2023-02-04 by me, and also at 2021 oct by a trusted friend.
     ("8185E158A33330C7FD61BC0D1F76E155CEFBA71C")
     ,guix-system-name->rust-system-name
     ("x86_64-linux"))))

(define (package-db-entry name)
  (assoc-ref +package-db+ name))

(define *package-db-entry* (make-parameter '()))

(define (download-uri-to-file uri file)
  ;; TODO maybe use guile-curl instead:
  ;; https://github.com/spk121/guile-curl
  (format #t "Downloading file ~A~%  from ~A~%" file uri)
  ;; TODO FIXME http 404 error messages should be deleted, otherwise continuing next time will leave trash in the files
  (system (string-append "curl --continue-at - --location " uri " --output " file)))

(define* (download-and-verify-arch arch url-factory-args)
  (format #t "~A\t\t" arch)
  (let* ((fingerprints (second (*package-db-entry*)))
         (uri-factory  (first  (*package-db-entry*)))
         (data-file    (tmp-file arch)))
    (download-uri-to-file (apply uri-factory arch url-factory-args)
                          data-file)
    (if (zero? (length fingerprints))
        (format #t "no fingerprints specified, skipping signature verification~%")
        (let ((sig-file (tmp-file arch ".sig")))
          (download-uri-to-file (apply uri-factory arch (append url-factory-args
                                                                '(#:suffix ".asc")))
                                sig-file)
          (verify-gpg-signature
           fingerprints
           (map (cut string-append (dirname (current-filename))
                     "/../keys/" <>)
                fingerprints)
           sig-file data-file)))
    (format #t "~%")))

(define* (hash-arch arch)
  (let* ((data-file (tmp-file arch))
         (hash (call-with-input-file data-file
                 (lambda (port)
                   (port-hash (hash-algorithm sha256) port))
                 #:binary #t)))
    (bytevector->nix-base32-string hash)))

(define (main cmd)
  (if (> (length cmd) 2)
      (let* ((pkg-name (second cmd))
             (url-factory-args (cddr cmd))
             (version (first url-factory-args)))
        ;; alternatively: (mkdtemp "/tmp/update-helper.XXXXXX")
        (parameterize ((*tmp-directory* (string-append "/tmp/" pkg-name "-" version))
                       (*package-db-entry* (package-db-entry (second cmd))))
          (mkdir-p (*tmp-directory*))
          (dynamic-wind
            (const #t)
            (lambda ()
              (let* ((guix-archs (fourth (*package-db-entry*)))
                     (upstream-archs (map (third (*package-db-entry*)) guix-archs)))
                (format #t "~%Fetching files and verifying signatures; version ~A~%~%" url-factory-args)
                (map (cut download-and-verify-arch <> url-factory-args) upstream-archs)
                (format #t "Hashing the files~%")
                (let ((db '()))
                  (for-each
                   (lambda (upstream-arch guix-arch)
                     (format #t "Hashing for arch ~A~%" upstream-arch)
                     (set! db (cons (cons guix-arch (hash-arch upstream-arch)) db)))
                   upstream-archs guix-archs)
                  (let ((db-file (string-append (dirname (current-filename))
                                                "/../src/guix-crypto/packages/hashes/"
                                                pkg-name ".hashes")))
                    (false-if-exception (delete-file db-file))
                    (with-output-to-file db-file
                      (lambda ()
                        (format #t ";; This file was generated by the ~A script~%"
                                (basename (current-filename)))
                        (write db)))))
                (format #t "Finished successfully~%")))
            (lambda ()
              ;; when using mkdtemp above: (delete-file-recursively (*tmp-directory*))
              #t))))
      (format #t "Usage: ~A pkg version [commit-hash, other args for the release uri factory]~%" (first cmd))))
