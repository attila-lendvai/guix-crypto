#!/usr/bin/env -S guile --no-auto-compile -e '(@@ (clef-pipe-test) main)' -s
!#

(add-to-load-path (string-append (dirname (current-filename)) "/../src"))

(define-module (clef-pipe-test)
 #:use-module (guix build utils)
 #:use-module (guix derivations)
 #:use-module (guix gexp)
 #:use-module (guix git-download)
 #:use-module (guix packages)
 #:use-module (guix store)
 #:use-module (guix http-client)
 #:use-module (guix base32)

 #:use-module (gnu packages bash)

 #:use-module (guix-crypto utils)
 #:use-module (guix-crypto swarm-utils)
 #:use-module (guix-crypto clef-utils)
 #:use-module (guix-crypto package-utils)
 #:use-module (guix-crypto script-utils)
 #:use-module (guix-crypto packages ethereum)

 #:use-module (srfi srfi-1)
 #:use-module (srfi srfi-11)
 #:use-module (srfi srfi-26)
 #:use-module (srfi srfi-34)
 #:use-module (srfi srfi-35)
 #:use-module (srfi srfi-60)
 #:use-module (ice-9 match)
 #:use-module (ice-9 popen)
 #:use-module (ice-9 rdelim)
 #:use-module (ice-9 regex)
 #:use-module (ice-9 format)
 #:use-module (rnrs io ports)

 #:use-module (gcrypt hash)
 #:use-module ((fibers) #:hide (sleep)))

(define* (pkg->output store pkg #:optional (output "out"))
  (let ((derivation (package-derivation store pkg)))
    (derivation->output-path derivation output)))

(define* (pkg-path store pkg path #:optional (output "out"))
  (string-append (pkg->output store pkg output) path))

(define* (var-lib-path relative-path)
  (string-append "/home/alendvai/workspace/guix/var-lib-of-guest-vm"
                 relative-path))

(define* (copy-lines input output)
  (let loop ()
    (let ((line (read-line input)))
      (cond
       ((eof-object? line)
        (close-port input))
       (else
        (put-string output line)
        (put-char output #\newline)
        (force-output output)
        (loop))))))

(define (main cli-args)
  (with-log-directory "/tmp"
    (set! *service-data-directory* "/tmp/swarm/")
    (with-store store
      (let* ((swarm-name "mainnet")
             (clef (pkg-path store geth-binary "/bin/clef" "clef"))
             (handle-stderr? #false) ; untested
             (args
              `("--stdio-ui"
                "--suppress-bootwarn"
                "--keystore" ,(clef-keystore-directory swarm-name)
                "--configdir" ,(clef-data-directory swarm-name)
                "--chainid" "100"
                "--rules" "/gnu/store/7dj69wzrfpnmqqah81424aa94qvskk11-bee-clef-v0.13.2-checkout/packaging/rules.js"
                "--nousb"
                "--lightkdf"
                "--4bytedb-custom" "/gnu/store/7dj69wzrfpnmqqah81424aa94qvskk11-bee-clef-v0.13.2-checkout/packaging/4byte.json"
                "--pcscdpath" ""
                "--auditlog" ""
                "--loglevel" "3"
                "--ipcpath" ,(clef-data-directory swarm-name)))
             (bee-index (if (< 1 (length cli-args))
                            (string->number (second cli-args))
                            (error "specify the desired bee-index on the cmd line!"))))
        (format #t "Starting clef, args are '~A'~%" args)

        (ensure-directories (getuid) (getgid) #o2775
                            (bee-data-directory swarm-name bee-index)
                            (clef-data-directory swarm-name))

        (unless (file-exists? (clef-password-file swarm-name))
          (with-output-to-file (clef-password-file swarm-name)
            (lambda ()
              (display "almakortebanan"))))

        (let* ((p1     (pipe O_NONBLOCK))
               (p2     (pipe O_NONBLOCK))
               (p3     (when handle-stderr?
                         (pipe O_NONBLOCK)))
               (password (read-file-to-string (clef-password-file swarm-name)))
               (clef-pid (apply spawn clef (cons clef args)
                                #:environment (cons (string-append "CLEF_PASSWORD="
                                                                   password)
                                                    (environ))
                                #:input  (car p1)
                                #:output (cdr p2)
                                (if handle-stderr?
                                    (list #:error (cdr p3))
                                    '()))))
          ;; (setvbuf (car p1) 'none)
          (dynamic-wind
            (const #f)

            (run-fibers
             (lambda ()
               (when handle-stderr?
                 (spawn-fiber
                  (lambda _
                    (copy-lines (car p3) (current-error-port)))))

               (spawn-clef-stdio-fiber clef-pid (car p2) (cdr p1) password)
               (format #t "stdio fiber spawned~%")

               (ensure-directories (getuid) (getgid) #o2775
                                   (bee-data-directory swarm-name bee-index))

               (let ((s (socket PF_UNIX SOCK_STREAM 0)))
                 (connect s AF_UNIX (clef-ipc-file swarm-name))
                 (make-port-non-blocking! s)
                 (ensure-clef-account* s swarm-name bee-index)
                 (close s))
               (format #t "exiting~%")
               (sleep 5)
               ;; TODO the return value gets apply'd!? by whom?
               'zork)
             #:parallelism 1
             #:hz 0)

            (lambda ()
              (kill clef-pid SIGINT)
              ;; close the pipe ports?
              )))))))
