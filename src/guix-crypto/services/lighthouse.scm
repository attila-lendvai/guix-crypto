;;; Copyright © 2022 Attila Lendvai <attila@lendvai.name>
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

;; TODO delme
;; /gnu/store/jkxm33y87b0p6ih4hp0r5h5jjlr1ah34-lighthouse-binary-3.4.0/bin/lighthouse --network gnosis beacon_node --datadir=/tmp/lighthouse --http --execution-endpoint http://localhost:8551 --execution-jwt /tmp/jwt.hex --checkpoint-sync-url "https://checkpoint.gnosischain.com"

;; TODO port this also to full user-account objects to properly set the
;; supplementary-groups.

(define-module (guix-crypto services lighthouse)
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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu services)
  #:use-module (gnu services admin)
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
  #:export (lighthouse-service-configuration
            lighthouse-service-configuration?
            lighthouse-configuration
            lighthouse-configuration?
            lighthouse-service-type
            lighthouse-service))

;;;
;;; Configuration
;;;
(define (lighthouse-command? val)
  (member val '(account_manager
                beacon_node
                boot_node
                database_manager
                validator_client)))

(define (serialize-field field-name value)
  (cond
   ((eq? field-name 'extra-arguments)
    (map (lambda (entry)
           (first (serialize-field (first entry) (second entry))))
         value))
   ((eq? value #true)
    ;; This is here for boolean fileds in EXTRA-ARGUMENTS. Otherwise
    ;; SERIALIZE-FIELD/BOOLEAN covers reified boolean fields.
    (list (string-append "--" (if (symbol? field-name)
                                  (symbol->string field-name)
                                  field-name))))
   ((eq? value #false)
    ;; ditto. we do nothing.
    )
   (else
    (list (string-append "--" (if (symbol? field-name)
                                  (symbol->string field-name)
                                  field-name)
                         "="
                         (if (string? value)
                             value
                             (object->string value)))))))

(define (serialize-field/boolean field-name val)
  (if val
      (list (string-append "--" (symbol->string field-name)))
      '()))

(define serialize-string  serialize-field)
(define serialize-list    serialize-field)
(define serialize-boolean serialize-field/boolean)
(define serialize-integer serialize-field)
(define serialize-non-negative-integer serialize-field)
(define serialize-service-name serialize-field)

(define-maybe string)
(define-maybe list)
(define-maybe boolean)
(define-maybe integer)
(define-maybe non-negative-integer)
(define-maybe service-name)

;; https://lighthouse.github.io/Configuring-Lighthouse
(define-configuration lighthouse-configuration
  ;; For simplicity the field names here are the same as the
  ;; Lighthouse config entry names.
  (command               (lighthouse-command 'beacon_node)
   "The command for the lighthouse binary."
   empty-serializer) ; it's special-cased in LIGHTHOUSE-CONFIGURATION->CMD-ARGUMENTS
  (datadir               maybe-string
                         "Directory where the state is stored.")
  (debug-level           maybe-string
   "possible values: info, debug, trace, warn, error, crit")
  (log-format            maybe-string
   "")
  (logfile               maybe-string ; TODO type: path
   "")
  (logfile-debug-level   maybe-string
   "possible values: info, debug, trace, warn, error, crit")
  (logfile-format        maybe-string
   "")
  (logfile-max-size      (maybe-non-negative-integer 8)
   "The maximum size (in MB) each log file can grow to before rotating. If set to 0, background file logging is disabled.")
  (logfile-max-number    (maybe-non-negative-integer 10)
   "Number of rotations to retain.")
  (logfile-compress      (maybe-boolean #true)
   "")
  (network               maybe-string
   "Which blockchain to connect to. Possible values: mainnet, prater, goerli, gnosis, sepolia")
  (extra-arguments          maybe-list
                            "A list of (name value) pairs that will be appended as-is to the end of the generated command line."))

(define-configuration/no-serialization lighthouse-service-configuration
  ;;
  ;; Guix packages
  ;;
  (lighthouse          (file-like lighthouse-binary)
   "The Guix package to use.")
  ;;
  ;; Service's config
  ;;
  (service-name          maybe-service-name
   "The name of this Guix service, a symbol. It will be used as the \
PROVISION value of the Shepherd service, and as a path component \
in the data and log directories. You typically want to use here \
the same value you provided as NETWORK.")
  (user-account          user-account
   "USER-ACCOUNT object (as per (gnu system shadow)) specifying the unix user/group the service process should run under.")
  (requirement
   (list '(networking file-systems))
   "Guix service names that are appended to the REQUIREMENT field of the \
Shepherd service instance; i.e. here you can specify extra \
dependencies for the start order of the services. Typically you want to add \
the name of the execution engine's service here.")
  (respawn-limit
   (respawn-limit #false)
   "Respawn limit. By default keep respawning forever.")
  (respawn-delay
   (number 5)
   "Wait secs between respawns. Defaults to 5.")
  (stdio-logfile
   maybe-string
   "")
  (lighthouse-configuration
   lighthouse-configuration
   "Configuration for the Lighthouse binary."))

;; NOTE: We cannot easily write a config file for OE because it groups the
;; various parameters in the TOML file, and we don't have any reified
;; representation of that grouping.
(define (lighthouse-configuration->cmd-arguments config)
  (cons*
   (symbol->string (lighthouse-configuration-command config))
   (list-transduce (compose (base-transducer config)
                           tconcatenate)
                  rcons
                  lighthouse-configuration-fields)))

(define (apply-config-defaults config)
  (match-record config <lighthouse-service-configuration>
      (service-name (lighthouse-configuration lh-config))
    (match-record lh-config <lighthouse-configuration>
        (network datadir logfile)
      (let* ((network (ensure-string network))
             (service-name (ensure-string
                            (maybe-value service-name
                                         (string-append "lh-" network)))))
        (lighthouse-service-configuration
         (inherit config)
         (service-name service-name)
         (lighthouse-configuration
          (let ((datadir (ensure-string
                          (maybe-value datadir
                                       (string-append "/var/lib/lighthouse/"
                                                      service-name)))))
            (lighthouse-configuration
             (inherit lh-config)
             (network      network)
             (datadir      datadir)
             (logfile      (maybe-value logfile
                                        (string-append (default-log-directory)
                                                       "/" service-name ".log")))))))))))

;;;
;;;
;;;
(define (default-log-directory)
  "/var/log/lighthouse")

(define (make-shepherd-service config)
  (set! config (apply-config-defaults config))
  (with-service-gexp-modules '()
    (match-record config <lighthouse-service-configuration>
        (user-account service-name lighthouse lighthouse-configuration
                      requirement respawn-limit respawn-delay stdio-logfile)
      (match-record lighthouse-configuration <lighthouse-configuration>
          (network datadir)
        (list
         (shepherd-service
          (documentation (simple-format #f "An lighthouse node connecting to network '~A'"
                                        network))
          (provision (list (string->symbol service-name)))
          (requirement requirement)
          (respawn-limit respawn-limit)
          (respawn-delay respawn-delay)
          (modules +default-service-modules+)
          (start
           (let ((log-dir  (default-log-directory)))
             #~(lambda args
                 (let ((user  '#$(user-account-name  user-account))
                       (group '#$(user-account-group user-account)))
                   ;;(setenv "PATH" #$path)
                   (with-log-directory #$log-dir
                     (ensure-directories #false #false #o2775 "/var/log/lighthouse")

                     (log2.debug "Lighthouse service is starting up")

                     (ensure-directories/rec user group #o2751 #$datadir)

                     (define cmd '#$(cons*
                                     (file-append lighthouse "/bin/lighthouse")
                                     (lighthouse-configuration->cmd-arguments
                                      lighthouse-configuration)))

                     (log2.debug "Will exec ~S" cmd)

                     (define forkexec
                       (make-forkexec-constructor
                        cmd
                        #:user user
                        #:group group
                        #:supplementary-groups '#$(user-account-supplementary-groups user-account)
                        ;; TODO set up logrotation for this
                        #:log-file #$(maybe-value stdio-logfile "/dev/null")
                        #:environment-variables
                        (append
                         (list (string-append "HOME=" #$datadir)
                               ;;(string-append "PATH=" #$path)
                               "RUST_BACKTRACE=full")
                         +root-environment+)))

                     ;; We need to do this here, because we must not return from
                     ;; START until the daemon is properly up and running,
                     ;; otherwise any (requirement ...) specification on other
                     ;; services is useless.
                     (let ((pid (apply forkexec args)))
                       ;;(ensure-ipc-file-permissions pid #$ipc-path)
                       pid))))))
          (stop #~(make-kill-destructor #:grace-period 60))))))))

;; (define (make-unix-user-accounts service-config)
;;   ;; It's a tempting idea to include USER-ACCOUNT here, but then what to do with
;;   ;; the group? The GROUP slot is only a string (i.e. not a USER-GROUP
;;   ;; instance).
;;   (list
;;    (user-group
;;     (name "lighthouse")
;;     (system? #t))))

(define (make-lighthouse-log-rotations service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <lighthouse-service-configuration>
      (service-name (lighthouse-configuration lh-config))
    (begin ;;match-record lh-config <lighthouse-configuration>
        ;;(network datadir logfile)
      (let ((log-dir (default-log-directory)))
          (list
           (log-rotation
            (files (list
                    (string-append log-dir "/service.log")
                    ;; TODO shall we rotate, or leave it to lighthouse?
                    ;; logfile
                    ))
            (frequency 'weekly)
            (options '("rotate 8"))))))))

;;
;; Interfacing with Guix
;;
(define lighthouse-service-type
  (service-type
   (name 'lighthouse)
   (extensions
    ;; We can't use the ACTIVATION-SERVICE-TYPE here, because it executes
    ;; us too early, before e.g. the filesystems are mounted in case of `guix
    ;; system --share=... vm ...`
    (list (service-extension shepherd-root-service-type
                             make-shepherd-service)
          ;; (service-extension account-service-type
          ;;                    make-unix-user-accounts)
          (service-extension rottlog-service-type
                             make-lighthouse-log-rotations)))
   (description "Runs an Lighthouse instance as a Shepherd service.")))
