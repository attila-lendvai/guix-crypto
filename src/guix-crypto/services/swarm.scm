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

;; TODO
;; - Use XDG_STATE_HOME for log files to support non-root shepherd?
;;   - https://issues.guix.gnu.org/53781
;; - make sure that passwords on the stack are not printed in backtraces
;; - synchronize our config defaults with that of the Bee client?

;; BUGS:
;; - chown the bee-0.password file?

(define-module (guix-crypto services swarm)
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto swarm-utils)
  #:use-module (guix-crypto service-utils)
  #:use-module (guix-crypto packages swarm)
  #:use-module (guix-crypto packages ethereum)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
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
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (rnrs hashtables)
  #:export (swarm-service-configuration
            swarm-service-configuration?
            bee-configuration
            bee-configuration?
            bee-config-values
            indexed-bee-configuration
            indexed-bee-configurations
            swarm-service-type
            swarm-service
            swarm/mainnet
            swarm/testnet))

(define* (hashtable-ref* table key #:optional (default #f))
  (let* ((missed-value table)
         (result (hashtable-ref table key missed-value)))
    (if (eq? result missed-value)
        (or default (error "Missing bee config value for key:" key))
        result)))

(define (swarm-name? val)
  ;; TODO assert that it's compatible with file paths
  (string? val))

(define-maybe/no-serialization string)
(define-maybe/no-serialization boolean)
(define-maybe/no-serialization integer)
(define-maybe/no-serialization non-negative-integer)
(define-maybe/no-serialization user-account)
(define-maybe/no-serialization user-group)

;; (define (serialize-field field-name val)
;;   (format #t "~A: ~A\n" field-name (value->string val)))
;; (define serialize-string  serialize-field)
;; (define serialize-boolean serialize-field)
;; (define serialize-integer serialize-field)
;; (define serialize-non-negative-integer serialize-field)

;;;
;;; Swarm Configuration
;;;
(define-configuration/no-serialization swarm
  (name                  swarm-name "")
  (network-id            maybe-non-negative-integer ""))

(define swarm/mainnet
  (swarm
   (name                "mainnet")
   (network-id          1)))

(define swarm/testnet
  (swarm
   (name                "testnet")
   (network-id          10)))

;;;
;;; Bee Configuration
;;;

(define-configuration/no-serialization bee-configuration
  (name                  symbol
                         "Primarily it's the name (provision) of the Shepherd service.")
  (config-values         hashtable
                         "A symbol -> value map that is emitted as-is into the bee yaml config file. It can be instantiated as (bee-config-values (key1 value1) (k2 v2) ...)."))

(define list-of-bee-configurations? (list-of bee-configuration?))

(define (list->bee-config-values entries)
  (let ((result (make-eq-hashtable)))
    (map (lambda (entry)
           (let ((key (first entry))
                 (value (second entry)))
             (hashtable-set! result key value)))
         entries)
    result))

(define-syntax bee-config-values
  (syntax-rules ()
    ((_ (key value) rest ...)
     (list->bee-config-values '((key value) rest ...)))
    ((_ ())
     (list->bee-config-values '()))))

(define (bee-configuration->yaml-string bee-configuration)

  (define (value->string val)
    (cond
     ((boolean? val)
      (if val "true" "false"))
     ((number? val)
      (string-append "\"" (number->string val) "\""))
     ((string? val)
      val)
     ((symbol? val)
      (symbol->string val))
     (else
      (error "Swarm VALUE->STRING: don't know how to deal with the value:" val))))

  (match-record bee-configuration <bee-configuration>
      (config-values)
    (with-output-to-string
      (lambda ()
        (vector-for-each
         (lambda (_ key)
           (format #t "~A: ~A\n" key
                   (value->string (hashtable-ref* config-values key))))
         (hashtable-keys config-values))))))

;;;
;;; Service Configuration
;;;
(define-configuration/no-serialization swarm-service-configuration
  (swarm                 (swarm swarm/mainnet)
   "An instance of <swarm> that describes the parameters of the swarm the \
nodes should join. Defaults to swarm/mainnet.")
  (bee                   (file-like bee-binary)
                         "The Bee Guix package to use.")
  (requirement
   (list '(networking file-systems))
   "Guix service names that are appended to the REQUIREMENT field of each \
Bee node's Shepherd service instance; i.e. here you can specify extra \
dependencies for the start order of the services, e.g. if you are running \
a local Gnosis chain node instance, then you can add its name here.")
  (respawn-limit
   (respawn-limit #false)
   "Respawn limit. By default keep respawning forever.")
  (respawn-delay
   (number 10)
   "Wait secs between respawns. Defaults to 5.")
  (bee-configurations   list-of-bee-configurations
                        "Instances of BEE-CONFIGURATION created e.g. by INDEXED-BEE-CONFIGURATION.")
  ;; Users and groups
  (unix-user            maybe-user-account
                        "USER-ACCOUNT object specifying the Unix user for the Bee processes.")
  (unix-group           maybe-user-group
                        "Unix group for the users in this swarm. Defaults to a system group with name 'swarm-[swarm-name]'"))

(define (apply-config-defaults service-config)
  (match-record service-config <swarm-service-configuration>
      (swarm unix-user unix-group)
    (match-record swarm <swarm>
        ((name swarm-name))
      (let* ((unix-group (maybe-value
                          unix-group
                          (user-group
                           (name (string-append "swarm-" swarm-name))
                           (system? #t))))
             (unix-user (maybe-value
                         unix-user
                         (user-account
                          (name (string-append "bee-" swarm-name))
                          (group (user-group-name unix-group))
                          (supplementary-groups '("swarm"))
                          (system? #t)
                          (comment (string-append "Swarm Bee service user for swarm " swarm-name))
                          (home-directory (swarm-data-directory swarm-name))
                          (shell (file-append shadow "/sbin/nologin"))))))
        ;; Return with a copy in which everything is fully specified.
        (swarm-service-configuration
         (inherit service-config)
         (unix-group unix-group)
         (unix-user unix-user))))))

(define* (indexed-bee-configuration node-index defaults #:key
                                    (swarm swarm/mainnet)
                                    listen-address
                                    (p2p-port-base 1600)
                                    (p2p-listen-address listen-address)
                                    (api-port-base 1700)
                                    (api-listen-address listen-address)
                                    (debug-api-port-base 1800)
                                    (debug-api-listen-address "localhost"))

  (define (as-addr-string address port-base)
    (string-append (or address "") ":" (number->string (+ port-base node-index))))

  (match-record swarm <swarm> ((name swarm-name))
    (let ((config-values (hashtable-copy defaults #true))
          (bee-name (string->symbol
                     (string-append "bee-"
                                    (if (equal? swarm-name "mainnet")
                                        ""
                                        (string-append swarm-name "-"))
                                    (number->string node-index)))))

      (define (set key value)
        (hashtable-set! config-values key value))

      (set 'p2p-addr       (as-addr-string p2p-listen-address p2p-port-base))
      (set 'api-addr       (as-addr-string api-listen-address api-port-base))
      (set 'debug-api-addr (as-addr-string debug-api-listen-address debug-api-port-base))
      (set 'data-dir       (bee-data-directory swarm-name bee-name))
      (set 'password-file  (bee-password-file  swarm-name bee-name))

      (bee-configuration
       (name          bee-name)
       (config-values config-values)))))

(define* (indexed-bee-configurations node-count defaults . kwargs)
  (map (lambda (node-index)
         (apply indexed-bee-configuration node-index defaults kwargs))
       (iota node-count)))


;;;
;;; Service implementation
;;;
(define (make-shepherd-service/bee service-config bee-config)

  (with-service-gexp-modules '((guix build utils)
                               (guix-crypto swarm-utils))
    (match-record service-config <swarm-service-configuration>
        (swarm bee unix-user unix-group requirement respawn-limit respawn-delay)
      (match-record swarm <swarm>
          ((name swarm-name))
        (match-record bee-config <bee-configuration>
            ((name bee-name) config-values)

          (define* (config-value key #:optional (default #f))
            (hashtable-ref* config-values key default))

          ;; TODO add: cashout, withdraw, balances, settlements, backup-identity
          (let* ((data-dir    (config-value 'data-dir))
                 (config-file (plain-file
                               (simple-format #f "bee-config-~A.yaml" bee-name)
                               (bee-configuration->yaml-string bee-config)))
                 (log-file    (bee-log-filename (default-log-directory swarm-name)
                                                bee-name)))

            (define* (shepherd-action/bee name doc gexp)
              (shepherd-action
               (name name)
               (documentation doc)
               (procedure
                (swarm-service-action-gexp service-config gexp))))

            (define address-action
              (shepherd-action/bee
               'address
               "Print the Bee node's Ethereum address."
               #~(let ((filename #$(bee-wallet-file swarm-name bee-name)))
                   (display (wallet-file-address filename)))))

            (define log-file-action
              (shepherd-action/bee
               'log-file
               "Print the Bee node's log file path."
               #~(display '#$log-file)))

            (define backup-identity-action
              (shepherd-action/bee
               'backup-identity
               "Backs up the Bee node's identity (everything except
the stored chunks) into the directory specified as the
first command line argument."
               #~(cond
                  ((= 1 (length -args-))
                   (cond
                    ((not (directory-exists? (first -args-)))
                     (format #t "Destination directory '~A' does not exists~%"
                             (first -args-)))
                    (else
                     (let* ((data-dir '#$data-dir)
                            (dest-dir (canonicalize-path (first -args-)))
                            (file-name (string-append (date->string (current-date) "~1") "-"
                                                      (gethostname)
                                                      "-bee-"
                                                      #$swarm-name "-"
                                                      (symbol->string '#$bee-name)))
                            (dest-path (string-append dest-dir "/" file-name ".tgz"))
                            ;; Let's record the fact that we depend on gzip.
                            (compressor #$(file-append gzip "/bin/gzip")))
                       (format #t "Backing up Bee node identity from '~A' to '~A'~%"
                               data-dir dest-path)
                       ;; TODO add a blacklist instead of the current whitelist
                       ;; TODO add the password file? and print a warning?
                       (let* ((cmd (list #$(file-append tar "/bin/tar")
                                         "--verbose"
                                         "--create"
                                         ;; "--file" dest-path
                                         "--directory" data-dir
                                         "keys/"
                                         "statestore/"))
                              (cmd-string (string-append
                                           #$(file-append coreutils "/bin/install")
                                           " --mode=600 <("
                                           (string-join cmd " ")
                                           " | " compressor ") " dest-path)))
                         (log.debug "Will run backup cmd: ~S, PATH is ~A, uid is ~A, $SHELL is ~S" cmd-string (getenv "PATH") (getuid) (getenv "SHELL"))
                         ;; NOTE SYSTEM* together with --gzip didn't work (status (apply system* cmd))
                         ;; NOTE SYSTEM is rebound in shepherd with SPAWN-SHELL-COMMAND,
                         ;; and it's not fully compatible: (system) errors instead of
                         ;; returning a boolean.
                         (let* ((status (system cmd-string))
                                (exit-code (status:exit-val status)))
                           (log.debug "Cmd returned exit code ~S" exit-code)
                           (if (zero? exit-code)
                               (format #t "Node identity files have been backed up into '~A'~%"
                                       dest-path)
                               (begin
                                 (format #t "Node identity backup command failed with exit-code '~S'~%"
                                         exit-code)
                                 (false-if-exception (delete-file dest-path))))))))))
                  (else
                   (format (current-error-port) "Usage: herd backup-identity bee-mainnet-0 destination-directory.~%")))))

            (shepherd-service
             (documentation (simple-format #f "Swarm bee ~S in swarm ~S."
                                           bee-name swarm-name))
             (provision (list bee-name))
             (requirement   requirement)
             (respawn?      #true)
             (respawn-limit respawn-limit)
             (respawn-delay respawn-delay)
             (actions (list address-action
                            log-file-action
                            (shepherd-configuration-action config-file)
                            backup-identity-action))
             (modules (append
                       '((guix-crypto swarm-utils)
                         (srfi srfi-1))
                       +default-service-modules+))
             (start
              #~(lambda _
                  #$(swarm-service-start-gexp
                     service-config
                     #~(let ((bee-name   '#$bee-name)
                             (swarm-name '#$swarm-name)
                             (data-dir   '#$data-dir)
                             (password-file '#$(config-value 'password-file)))
                         (log.debug "Bee service is starting")

                         (ensure-directories/rec unix-user-id bee-group-id #o2770 data-dir)
                         (ensure-password-file password-file unix-user-id bee-group-id)

                         (let ()
                           (define (spawn-bee* action)
                             (let ((cmd (list #$(file-append bee "/bin/bee")
                                              "--config" #$config-file
                                              action)))
                               (log.dribble "About to spawn Bee, cmd is: ~S" cmd)
                               (fork+exec-command
                                cmd
                                #:user '#$(user-account-name unix-user)
                                #:group '#$(user-group-name unix-group)
                                #:log-file '#$log-file
                                #:directory data-dir
                                #:resource-limits
                                (let ((open-files-limit
                                       (+ #$(config-value 'db-open-files-limit 4096)
                                          4096))) ; arbitrary extra for the rest
                                  `((nofile ,open-files-limit ,open-files-limit)))
                                #:environment-variables
                                (delete #false
                                        (append
                                         (list
                                          (string-append "HOME=" data-dir)
                                          ;; So that these are not visible with ps, or in the
                                          ;; config file (i.e. world-readable under
                                          ;; /gnu/store/), because they may contain keys when
                                          ;; using a service like Infura.
                                          (let ((value '#$(config-value 'blockchain-rpc-endpoint)))
                                            (and value
                                                 (string-append "BEE_BLOCKCHAIN_RPC_ENDPOINT=" value)))
                                          (let ((value '#$(config-value 'resolver-options)))
                                            (and value
                                                 (string-append "BEE_RESOLVER_OPTIONS=" value))))
                                         +root-environment+)))))

                           (define (bee-already-initialized?)
                             (any (lambda (el)
                                    (file-exists?
                                     (string-append data-dir "/keys/" el)))
                                  '("libp2p_v2.key"
                                    "swarm.key"
                                    "pss.key")))

                           ;; When first started, call `bee init` for this bee
                           ;; instance.
                           (unless (bee-already-initialized?)
                             (log.debug "Invoking `bee init` for bee ~S in swarm ~S" bee-name swarm-name)
                             (wait-for-pid
                              (spawn-bee* "init"))
                             (log.dribble "`bee init` finished for bee ~S in swarm ~S" bee-name swarm-name))

                           ;; Due to staged compilation, we cannot add the node's
                           ;; eth address to the config bee file, because it only
                           ;; gets generated at service runtime. Hence we're
                           ;; passing it as an env variable.
                           (spawn-bee* "start"))))))
             (stop #~(make-kill-destructor #:grace-period 120)))))))))

(define (make-swarm-shepherd-services service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (bee-configurations)
    (map (lambda (bee-config)
           (make-shepherd-service/bee service-config bee-config))
         bee-configurations)))

(define (swarm-service-start-gexp service-config body-gexp)
  "Returns a GEXP that is called before the start of any of the services.  It
means that this GEXP should only do operations that are safe to be called any
number of times, in any random moment."
  (match-record service-config <swarm-service-configuration>
      (swarm unix-user unix-group)
    (match-record swarm <swarm>
        ((name swarm-name))
      ;; TODO use the -foo- naming convention for the implicit bindings below?
      ;; TODO simplify this, and maybe split into two for the actions
      #~(let* ((swarm-name    #$swarm-name)
               (path          #$(file-append coreutils "/bin"))
               (bash          #$(file-append bash-minimal "/bin/bash"))
               (tr            #$(file-append coreutils "/bin/tr"))
               (head          #$(file-append coreutils "/bin/head"))
               (chown-bin     #$(file-append coreutils "/bin/chown"))
               ;; reduce/rebind the USER-ACCOUNT and USER-GROUP instances into their names
               (unix-group    #$(user-group-name unix-group))
               (unix-user     #$(user-account-name unix-user))
               (unix-pw       (getpwnam unix-user))
               (unix-user-id  (passwd:uid unix-pw))
               (bee-group-id  (passwd:gid unix-pw))
               (log-dir       #$(default-log-directory swarm-name)))

          ;; so that we can already invoke basic commands before the fork+exec
          (setenv "PATH" path)
          (with-log-directory log-dir
            ;; Log files are not visible to everyone.
            (ensure-directories/rec unix-user unix-group #o2770
                                    log-dir)

            ;; Ensure as root that the service.log file exists, and it is
            ;; group writable.
            (let ((path (service-log-filename)))
              (close-port (open-file path "a"))
              (chmod path #o664))

            (log.dribble "A SWARM-SERVICE-START-GEXP is running for swarm ~S on Guile version ~S" swarm-name (version))

            (let ((dir #$(swarm-data-directory swarm-name)))
              ;; The data dir is visible to everyone.
              (ensure-directories 0 unix-group #o2770 dir)
              (log.debug "Ensured directory ~S" dir))

            #$body-gexp)))))

;; TODO factor out? this is a stripped down version of the above.
(define (swarm-service-action-gexp service-config body-gexp)
  (match-record service-config <swarm-service-configuration>
      (swarm unix-user unix-group)
    (match-record swarm <swarm>
      ((name swarm-name))
      #~(begin
          ;; TODO this srfi-19 should be somewhere more locally scoped,
          ;; but i don't know how to make that work.
          (use-modules (srfi srfi-19))

          (lambda (-pid- . -args-)
            (let* ((-path-          (string-append #$(file-append coreutils "/bin")
                                                   ":"
                                                   #$(file-append gzip "/bin")))
                   (-log-dir-       #$(default-log-directory swarm-name)))
              (setenv "PATH" -path-)
              (with-log-directory -log-dir-
                ;; Log files are not visible to everyone.
                (ensure-directories/rec #$(user-account-name unix-user)
                                        #$(user-group-name unix-group)
                                        #o2770
                                        -log-dir-)
                #$body-gexp)))))))

(define (make-swarm-user-accounts service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (swarm unix-user unix-group)
    (match-record swarm <swarm>
        ((name swarm-name))
      (list (user-group
             (name "swarm")
             (system? #t))
            unix-group
            unix-user))))

(define (make-swarm-log-rotations service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (swarm bee-configurations)
    (match-record swarm <swarm>
        ((name swarm-name))
      (let ((log-dir (default-log-directory swarm-name)))
        (list
         (log-rotation
          (files (cons*
                  (string-append log-dir "/service.log")
                  (map (lambda (bee-cfg)
                         (bee-log-filename log-dir (bee-configuration-name bee-cfg)))
                       bee-configurations)))
          (frequency 'weekly)
          (options '("rotate 8"))))))))

;;
;; Interfacing with Guix
;;
(define swarm-service-type
  (service-type
   (name 'swarm)
   (extensions
    ;; We can't extend the ACTIVATION-SERVICE-TYPE here, because it executes
    ;; us too early, before e.g. the filesystems are mounted in case of `guix
    ;; system --share=... vm ...`
    (list (service-extension shepherd-root-service-type
                             make-swarm-shepherd-services)
          (service-extension account-service-type
                             make-swarm-user-accounts)
          (service-extension rottlog-service-type
                             make-swarm-log-rotations)))
   (description "Runs the requested number of Bee instances as Shepherd services.")))
