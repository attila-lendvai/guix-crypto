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

;;; Note that OpenEthereum will soon be EOL'd:
;;; https://blog.alchemy.com/blog/openethereum-closing-shop

(define-module (guix-crypto services openethereum)
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto service-utils)
  #:use-module (guix-crypto packages ethereum)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module ((guix records) #:hide (match-record)) ;; TODO temporarily
  #:use-module (guix packages)
  ;;#:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration2)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu system accounts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (openethereum-service-configuration
            openethereum-service-configuration?
            openethereum-configuration
            openethereum-configuration?
            openethereum-service-type
            openethereum-service))

;;;
;;; Configuration
;;;
(define (serialize-field field-name val)
  (list (string-append "--" (symbol->string field-name))
        (if (string? val)
            val
            (object->string val))))

(define (serialize-field/boolean field-name val)
  (if val
      (list (string-append "--" (symbol->string field-name)))
      '()))

(define serialize-string  serialize-field)
(define serialize-boolean serialize-field/boolean)
(define serialize-integer serialize-field)
(define serialize-non-negative-integer serialize-field)
(define serialize-service-name serialize-field)
;; (define (serialize-list field-name val)
;;   (map (cut serialize-field field-name <>) val))

(define-maybe string)
(define-maybe boolean)
(define-maybe integer)
(define-maybe non-negative-integer)

;; https://openethereum.github.io/Configuring-OpenEthereum
(define-configuration openethereum-configuration
  ;; For simplicity the field names here are the same as the
  ;; OpenEthereum config entry names.
  (chain                 (maybe-string)
   "Which blockchain to connect to. It may be the name of a supported \
chain or a JSON file path.")
  (base-path             (maybe-string)
   "Directory where the state is stored.")
  (ipc-path              (maybe-string)
   "File name of the IPC file.")
  (identity              (maybe-string)
   "")
  (scale-verifiers       (maybe-boolean)
   "")
  (warp-barrier          (maybe-non-negative-integer)
   "")
  (no-ws                 (maybe-boolean)
   "Disable the websocket interface.")
  (no-jsonrpc            (maybe-boolean)
   "Disable the jsonrpc interface.")
  (no-ipc                (maybe-boolean)
   "Disable the file based IPC interface.")
  (unsafe-expose         (boolean #false)
   "")
  (ws-interface          maybe-string "")
  (ws-port               maybe-non-negative-integer "")
  (ws-hosts              maybe-string "")
  (jsonrpc-interface     maybe-string "")
  (jsonrpc-port          maybe-non-negative-integer "")
  (jsonrpc-hosts         maybe-string "")
  (enable-snapshotting   (maybe-boolean)
   "Create a snapshot every 5000 blocks that other nodes can download using warp syncing.")
  (ports-shift           (maybe-integer)
   "")
  (bootnodes             (maybe-string)
   "")
  (nat                   (maybe-string)
   "")
  (max-peers             (maybe-integer)
   "")
  (min-peers             (maybe-integer)
   "")
  (snapshot-peers        (maybe-integer)
   ""))

(define-configuration/no-serialization openethereum-service-configuration
  ;;
  ;; Guix packages
  ;;
  (openethereum          (file-like openethereum-binary)
   "The Guix package to use.")
  ;;
  ;; Service's config
  ;;
  ;; TODO validate it to be compatible with paths
  ;; TODO maybe rename to name
  (service-name          (service-name)
   "The name of this Guix service, a symbol. It will be used as the \
PROVISION value of the Shepherd service, and as a path component \
in the data and log directories. You typically want to use here \
the same value you provided as CHAIN.")
  (user                  (maybe-string)
   "Unix user for the service process.")
  (user-id               (maybe-non-negative-integer)
   "Unix uid for @code{user}.")
  (group                 (maybe-string)
   "Unix group for @code{user} and the service processes.")
  (group-id              (maybe-non-negative-integer)
   "Unix gid for @code{group}.")
  (openethereum-configuration (openethereum-configuration)
   "Configuration for the OpenEthereum binary."))

;; NOTE: We cannot easily write a config file for OE because it groups the
;; various parameters in the TOML file, and we don't have any reified
;; representation of that grouping.
(define (openethereum-configuration->cmd-arguments config)
  (fold (lambda (field result)
          (let ((name (configuration-field-name field))
                (value ((configuration-field-getter field) config)))
            (if (defined-value? value)
                (append ((configuration-field-serializer field) name value)
                        result)
                result)))
        '()
        openethereum-configuration-fields))

(define (apply-config-defaults config)
  (match-record config <openethereum-service-configuration>
      (user group service-name (openethereum-configuration oe-config))
    (match-record oe-config <openethereum-configuration>
        (chain base-path ipc-path)
      (let ((chain (ensure-string chain)))
        (openethereum-service-configuration
         (inherit config)
         (user         (or (defined-value? user)
                           (string-append "oe-" chain)))
         (group        (or (defined-value? group)
                           "openethereum"))
         (service-name (if (defined-value? service-name)
                           (ensure-string service-name)
                           (string-append "oe-" chain)))
         (openethereum-configuration
          (openethereum-configuration
           (inherit oe-config)
           (chain chain)
           (base-path    (if (defined-value? base-path)
                             (ensure-string base-path)
                             (string-append "/var/lib/openethereum/" service-name)))
           (ipc-path     (if (defined-value? ipc-path)
                             ipc-path
                             (string-append "/var/lib/openethereum/"
                                            service-name "/"
                                            service-name ".ipc"))))))))))

;;;
;;;
;;;
(define (default-log-directory)
  "/var/log/openethereum")

(define (openethereum-log-filename log-dir service-name)
  (simple-format #f "~A/~A.log" log-dir service-name))

(define (make-shepherd-service config)
  (set! config (apply-config-defaults config))
  (with-service-gexp-modules '()
    (match-record config <openethereum-service-configuration>
        (user group service-name openethereum openethereum-configuration)
      (match-record openethereum-configuration <openethereum-configuration>
          (chain base-path no-ipc ipc-path)
        (list
         (shepherd-service
          (documentation (simple-format #f "An openethereum node connecting to chain '~A'"
                                        chain))
          (provision (list (string->symbol service-name)))
          (requirement '(networking file-systems))
          (modules +default-service-modules+)
          (start
           (let ((path     (file-append coreutils "/bin"))
                 (log-dir  (default-log-directory)))
             #~(lambda args
                 (setenv "PATH" #$path)
                 (with-log-directory #$log-dir
                   (ensure-directories 0 "openethereum" #o2771
                                       "/var/lib/openethereum"
                                       "/var/log/openethereum")

                   (ensure-directories/rec #$user #$group #o2751
                                           #$base-path)

                   (log.debug "OpenEthereum service is starting up")

                   (define cmd '#$(cons*
                                   (file-append openethereum "/bin/openethereum")
                                   (openethereum-configuration->cmd-arguments
                                    openethereum-configuration)))

                   (log.debug "Will exec ~S" cmd)

                   (define forkexec
                     (make-forkexec-constructor
                      cmd
                      #:user #$user
                      #:group #$group
                      #:log-file #$(openethereum-log-filename log-dir service-name)
                      #:environment-variables
                      (list (string-append "HOME=" #$base-path)
                            (string-append "PATH=" #$path)
                            "LC_ALL=en_US.UTF-8")))

                   ;; We need to do this here, because we must not return from
                   ;; START until the daemon is properly up and running,
                   ;; otherwise any (requirement ...) specification on other
                   ;; services is useless.
                   (let ((pid (apply forkexec args)))
                     (when #$(or (undefined-value? no-ipc)
                                 (not no-ipc))
                       (ensure-ipc-file-permissions pid #$ipc-path))
                     pid)))))
          ;; TODO this should wait, herd restart fails now
          (stop #~(make-kill-destructor))))))))

(define (make-unix-user-accounts config)
  (set! config (apply-config-defaults config))
  (match-record
      config <openethereum-service-configuration>
    (user user-id group group-id openethereum-configuration)
    (match-record
          openethereum-configuration <openethereum-configuration>
      (chain base-path)
      ;; NOTE it's safe to forward the #false default value of uid/gid to
      ;; USER-ACCOUNT.
      (append
       (if (equal? group "openethereum")
           '()
           (list
            (user-group
             (name "openethereum")
             (system? #t))))
       (list
        (user-group
         (name group)
         (id (or (defined-value? group-id) #false))
         (system? #t))
        (user-account
         (name user)
         (uid (or (defined-value? user-id) #false))
         (group group)
         (supplementary-groups (delete group '("openethereum")))
         (system? #t)
         (comment (string-append "OpenEthereum service for chain '" chain "'"))
         (home-directory base-path)
         (shell (file-append shadow "/sbin/nologin"))))))))

(define (make-openethereum-log-rotations service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <openethereum-service-configuration>
      (service-name openethereum-configuration)
    (let ((log-dir (default-log-directory)))
      (list
       (log-rotation
        (files (list
                (string-append log-dir "/service.log")
                (openethereum-log-filename log-dir service-name)))
        (frequency 'weekly)
        (options '("rotate 8")))))))

;;
;; Interfacing with Guix
;;
(define openethereum-service-type
  (service-type
   (name 'openethereum)
   (extensions
    ;; We can't use the ACTIVATION-SERVICE-TYPE here, because it executes
    ;; us too early, before e.g. the filesystems are mounted in case of `guix
    ;; system --share=... vm ...`
    (list (service-extension shepherd-root-service-type
                             make-shepherd-service)
          (service-extension account-service-type
                             make-unix-user-accounts)
          (service-extension rottlog-service-type
                             make-openethereum-log-rotations)))
   (description "Runs an OpenEthereum instance as a Shepherd service.")))

(define* (openethereum-service #:key
                               (user                *unspecified*)
                               (group               *unspecified*)
                               (chain               "foundation")
                               (service-name        'openethereum)
                               (ipc-path            *unspecified*)
                               (warp-barrier        *unspecified*)
                               (min-peers           *unspecified*)
                               (max-peers           *unspecified*)
                               (snapshot-peers      *unspecified*)
                               (enable-snapshotting *unspecified*)
                               (scale-verifiers     #true)
                               (unsafe-expose       #false)
                               (no-ws               *unspecified*)
                               (no-jsonrpc          *unspecified*))
  (service openethereum-service-type
           (openethereum-service-configuration
            (service-name            service-name)
            (user                    user)
            (group                   group)
            (openethereum-configuration
             (openethereum-configuration
              (chain                 chain)
              (ipc-path              ipc-path)
              (warp-barrier          warp-barrier)
              (min-peers             min-peers)
              (max-peers             max-peers)
              (snapshot-peers        snapshot-peers)
              (enable-snapshotting   enable-snapshotting)
              (scale-verifiers       scale-verifiers)
              (unsafe-expose         unsafe-expose)
              (no-ws                 no-ws)
              (no-jsonrpc            no-jsonrpc))))))
