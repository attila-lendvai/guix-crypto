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
  #:use-module (guix records)
  #:use-module (guix packages)
  ;;#:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
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

(define (service-name? val)
  (and (defined-value? val) ; TODO delete this once srfi-189 Maybe is used for configs
       (or (symbol? val)
           (string? val))
       ;; TODO assert that it's a valid file path component
       ))

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

(define-configuration openethereum-configuration
  ;; For simplicity the field names here are the same as the
  ;; OpenEthereum config entry names.
  (chain                 (maybe-string 'disabled)
   "Which blockchain to connect to. It may be the name of a supported \
chain or a JSON file path.")
  (base-path             (maybe-string 'disabled)
   "Directory where the state is stored.")
  (ipc-path              (maybe-string 'disabled)
   "File name of the IPC file.")
  (identity              (maybe-string 'disabled)
   "")
  (scale-verifiers       (maybe-boolean 'disabled)
   "")
  (warp-barrier          (maybe-non-negative-integer 'disabled)
   "")
  (no-ws                 (maybe-boolean 'disabled)
   "Disable the websocket interface.")
  (no-jsonrpc            (maybe-boolean 'disabled)
   "Disable the jsonrpc interface.")
  (no-ipc                (maybe-boolean 'disabled)
   "Disable the file based IPC interface.")
  (ports-shift           (maybe-integer 'disabled)
   "")
  (bootnodes             (maybe-string 'disabled)
   "")
  (nat                   (maybe-string 'disabled)
   "")
  (max-peers             (maybe-integer 'disabled)
   "")
  (min-peers             (maybe-integer 'disabled)
   "")
  (snapshot-peers        (maybe-integer 'disabled)
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
  (user                  (maybe-string 'disabled)
   "Unix user for the service process.")
  (user-id               (maybe-non-negative-integer 'disabled)
   "Unix uid for @code{user}.")
  (group                 (maybe-string 'disabled)
   "Unix group for @code{user} and the service processes.")
  (group-id              (maybe-non-negative-integer 'disabled)
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
    (user group service-name)
    (let ((oe-config (openethereum-service-configuration-openethereum-configuration config)))
      (match-record oe-config <openethereum-configuration>
        (chain base-path ipc-path)
        (let ((chain (ensure-string chain)))
          (openethereum-service-configuration
           (inherit config)
           (user         (or (defined-value? user)
                             (string-append "oe-" chain)))
           (group        (or (defined-value? group)
                             (string-append "oe-" chain)))
           (service-name (if (defined-value? service-name)
                             (ensure-string service-name)
                             (string-append "openethereum-" chain)))
           (openethereum-configuration
            (openethereum-configuration
             (inherit oe-config)
             (chain chain)
             (base-path    (if (defined-value? base-path)
                               (ensure-string base-path)
                               (string-append "/var/lib/openethereum/" chain)))
             (ipc-path     (if (defined-value? ipc-path)
                               ipc-path
                               (string-append base-path "/jsonrpc.ipc")))))))))))

;;;
;;;
;;;
(define-public (default-service-name chain)
  ;; TODO what if CHAIN is a path to a JSON file?
  (string->symbol (simple-format #f "openethereum-~A" chain)))

(define (make-shepherd-service config)
  (set! config (apply-config-defaults config))
  (with-service-gexp-modules '()
    (match-record
        config <openethereum-service-configuration>
      (user group service-name openethereum openethereum-configuration)
      (match-record
          openethereum-configuration <openethereum-configuration>
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
                 (log-dir  "/var/log/openethereum"))
             #~(lambda args
                 (setenv "PATH" #$path)
                 (with-log-directory #$log-dir
                   (ensure-service-directories #$user #$group #$log-dir #$base-path)
                   (chown-r #$user #$group #$log-dir #$base-path)

                   (log.debug "OpenEthereum service is starting up")

                   (define forkexec
                     (make-forkexec-constructor
                      (cons*
                       #$(file-append openethereum "/bin/openethereum")
                       '#$(openethereum-configuration->cmd-arguments
                           openethereum-configuration))
                      #:user #$user
                      #:group #$group
                      #:log-file (string-append (*log-directory*)
                                                #$(simple-format #f "/~A.log" service-name))
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
      (list (user-group
             (name group)
             (id (or (defined? group-id) #false))
             (system? #t))
            (user-account
             (name user)
             (uid (or (defined? user-id) #false))
             (group group)
             (system? #t)
             (comment (string-append "OpenEthereum service for chain '" chain "'"))
             (home-directory base-path)
             (shell (file-append shadow "/sbin/nologin")))))))

;;
;; Interfacing with Guix
;;
(define openethereum-service-type
  (service-type
   (name 'openethereum)
   (extensions
    ;; We can't extend the ACTIVATION-SERVICE-TYPE here, because it executes
    ;; us too early, before e.g. the filesystems are mounted in case of `guix
    ;; system --share=... vm ...`
    (list (service-extension shepherd-root-service-type
                             make-shepherd-service)
          (service-extension account-service-type
                             make-unix-user-accounts)))))

(define* (openethereum-service #:key
                               (user 'disabled)
                               (group 'disabled)
                               (chain "foundation")
                               (service-name 'openethereum)
                               (warp-barrier 'disabled))
  (service openethereum-service-type
           (openethereum-service-configuration
            (service-name            service-name)
            (user                    user)
            (group                   group)
            (openethereum-configuration
             (openethereum-configuration
              (chain                 chain)
              (warp-barrier          warp-barrier)
              (scale-verifiers       #true))))))