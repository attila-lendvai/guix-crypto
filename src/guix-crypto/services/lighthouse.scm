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
(define-maybe service-name)

;; https://lighthouse.github.io/Configuring-Lighthouse
(define-configuration lighthouse-configuration
  ;; For simplicity the field names here are the same as the
  ;; Lighthouse config entry names.
  (network               maybe-string
   "Which blockchain to connect to.")
  (datadir               maybe-string
   "Directory where the state is stored.")
  (ipc-path              maybe-string
   "File name of the IPC file."))

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
  (user                  maybe-string
   "Unix user for the service process.")
  (user-id               maybe-non-negative-integer
   "Unix uid for @code{user}.")
  (group                 maybe-string
   "Unix group for @code{user} and the service processes.")
  (group-id              maybe-non-negative-integer
   "Unix gid for @code{group}.")
  (lighthouse-configuration lighthouse-configuration
   "Configuration for the Lighthouse binary."))

;; NOTE: We cannot easily write a config file for OE because it groups the
;; various parameters in the TOML file, and we don't have any reified
;; representation of that grouping.
(define (lighthouse-configuration->cmd-arguments config)
  (fold (lambda (field result)
          (let ((name (configuration-field-name field))
                (value ((configuration-field-getter field) config)))
            (if (maybe-value-set? value)
                (append ((configuration-field-serializer field) name value)
                        result)
                result)))
        '()
        lighthouse-configuration-fields))

(define (apply-config-defaults config)
  (match-record config <lighthouse-service-configuration>
      (user group service-name (lighthouse-configuration oe-config))
    (match-record oe-config <lighthouse-configuration>
        (network datadir ipc-path)
      (let* ((network (ensure-string network))
             (service-name (ensure-string
                            (maybe-value service-name
                                         (string-append "oe-" network)))))
        (lighthouse-service-configuration
         (inherit config)
         (user         (maybe-value user (string-append "oe-" network)))
         (group        (maybe-value group "lighthouse"))
         (service-name service-name)
         (lighthouse-configuration
          (let ((datadir (ensure-string
                          (maybe-value datadir
                                       (string-append "/var/lib/lighthouse/"
                                                      service-name)))))
            (lighthouse-configuration
             (inherit oe-config)
             (network      network)
             (datadir      datadir)
             (ipc-path     (maybe-value ipc-path
                                        (string-append datadir "/" service-name ".ipc")))))))))))

;;;
;;;
;;;
(define (default-log-directory)
  "/var/log/lighthouse")

(define (lighthouse-log-filename log-dir service-name)
  (simple-format #f "~A/~A.log" log-dir service-name))

(define (make-shepherd-service config)
  (set! config (apply-config-defaults config))
  (with-service-gexp-modules '()
    (match-record config <lighthouse-service-configuration>
        (user group service-name lighthouse lighthouse-configuration)
      (match-record lighthouse-configuration <lighthouse-configuration>
          (network datadir ipc-path)
        (list
         (shepherd-service
          (documentation (simple-format #f "An lighthouse node connecting to network '~A'"
                                        network))
          (provision (list (string->symbol service-name)))
          (requirement '(networking file-systems))
          (modules +default-service-modules+)
          (start
           (let ((path     (file-append coreutils "/bin"))
                 (log-dir  (default-log-directory)))
             #~(lambda args
                 (setenv "PATH" #$path)
                 (with-log-directory #$log-dir
                   (ensure-directories 0 "lighthouse" #o2771
                                       "/var/lib/lighthouse"
                                       "/var/log/lighthouse")

                   (ensure-directories/rec #$user #$group #o2751
                                           #$datadir)

                   (log.debug "Lighthouse service is starting up")

                   (define cmd '#$(cons*
                                   (file-append lighthouse "/bin/lighthouse")
                                   (lighthouse-configuration->cmd-arguments
                                    lighthouse-configuration)))

                   (log.debug "Will exec ~S" cmd)

                   (define forkexec
                     (make-forkexec-constructor
                      cmd
                      #:user #$user
                      #:group #$group
                      #:log-file #$(lighthouse-log-filename log-dir service-name)
                      #:environment-variables
                      (append
                       (list (string-append "HOME=" #$datadir)
                             (string-append "PATH=" #$path))
                       +root-environment+)))

                   ;; We need to do this here, because we must not return from
                   ;; START until the daemon is properly up and running,
                   ;; otherwise any (requirement ...) specification on other
                   ;; services is useless.
                   (let ((pid (apply forkexec args)))
                     (ensure-ipc-file-permissions pid #$ipc-path)
                     pid)))))
          ;; TODO this should wait, herd restart fails now
          (stop #~(make-kill-destructor))))))))

(define (make-unix-user-accounts config)
  (set! config (apply-config-defaults config))
  (match-record
      config <lighthouse-service-configuration>
    (user user-id group group-id lighthouse-configuration)
    (match-record
          lighthouse-configuration <lighthouse-configuration>
      (network datadir)
      ;; NOTE it's safe to forward the #false default value of uid/gid to
      ;; USER-ACCOUNT.
      (append
       (if (equal? group "lighthouse")
           '()
           (list
            (user-group
             (name "lighthouse")
             (system? #t))))
       (list
        (user-group
         (name group)
         (id (maybe-value group-id))
         (system? #t))
        (user-account
         (name user)
         (uid (maybe-value user-id))
         (group group)
         (supplementary-groups (delete group '("lighthouse")))
         (system? #t)
         (comment (string-append "Lighthouse service for network '" network "'"))
         (home-directory datadir)
         (shell (file-append shadow "/sbin/nologin"))))))))

(define (make-lighthouse-log-rotations service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <lighthouse-service-configuration>
      (service-name lighthouse-configuration)
    (let ((log-dir (default-log-directory)))
      (list
       (log-rotation
        (files (list
                (string-append log-dir "/service.log")
                (lighthouse-log-filename log-dir service-name)))
        (frequency 'weekly)
        (options '("rotate 8")))))))

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
          (service-extension account-service-type
                             make-unix-user-accounts)
          (service-extension rottlog-service-type
                             make-lighthouse-log-rotations)))
   (description "Runs an Lighthouse instance as a Shepherd service.")))

(define* (lighthouse-service #:key
                               (user                %unset-value)
                               (group               %unset-value)
                               (network             "mainnet")
                               (service-name        'lighthouse)
                               (ipc-path            %unset-value))
  (service lighthouse-service-type
           (lighthouse-service-configuration
            (service-name            service-name)
            (user                    user)
            (group                   group)
            (lighthouse-configuration
             (lighthouse-configuration
              (network               network)
              (ipc-path              ipc-path))))))
