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
;; - add respawn delay. test scenario: broken swap endpoint url makes it respawn in a busy loop
;;   - maybe add a respawn-delay to respawn-service in shepherd

;; BUGS:
;; - chown the bee-0.password file?
;; - herd backup-identity bee-0 creates an empty tgz

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
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (swarm-service-configuration
            swarm-service-configuration?
            bee-configuration
            bee-configuration?
            swarm-service-type
            swarm-service
            swarm/mainnet
            swarm/testnet))

(define +db-open-files-limit/default+ 4096)

(define (swarm-name? val)
  ;; TODO validate it to be compatible with file paths
  (string? val))

(define (verbosity-value? val)
  (or (member val '(silent error warn info debug trace))
      (and (integer? val)
           (<= 0 val 5))))

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

(define (serialize-field field-name val)
  (format #t "~A: ~A\n" field-name (value->string val)))

(define-maybe string)
(define-maybe boolean)
(define-maybe integer)
(define-maybe non-negative-integer)
(define-maybe user-account)
(define-maybe user-group)

(define serialize-string  serialize-field)
(define serialize-boolean serialize-field)
(define serialize-integer serialize-field)
(define serialize-non-negative-integer serialize-field)
(define serialize-verbosity-value serialize-field)
(define (serialize-raw-configuration-string field value)
  (when (maybe-value-set? value)
    (display value)))

(define raw-configuration-string? maybe-string?)

;;;
;;; Swarm Configuration
;;;
(define-configuration/no-serialization swarm
  (name                  swarm-name "")
  (network-id            maybe-non-negative-integer "")
  (p2p-port-base         non-negative-integer
   "Base number for the p2p ports of the Bee nodes.  Defaults to 1600 for \
@code{mainnet} and 1900 for @code{testnet}, otherwise it must be specified.")
  (api-port-base         non-negative-integer
   "Base number for the api ports of the Bee nodes.  Defaults to 1700 for \
@code{mainnet} and 2000 for @code{testnet}, otherwise it must be specified.")
  (debug-api-port-base   non-negative-integer
   "Base number for the debug api ports of the Bee nodes. Defaults to 1800 \
for @code{mainnet} and 2100 for @code{testnet}, otherwise it must be specified."))

(define swarm/mainnet
  (swarm
   (name                "mainnet")
   (network-id          1)
   (p2p-port-base       1600)
   (api-port-base       1700)
   (debug-api-port-base 1800)))

(define swarm/testnet
  (swarm
   (name                "testnet")
   (network-id          10)
   (p2p-port-base       11600)
   (api-port-base       11700)
   (debug-api-port-base 11800)))

;;;
;;; Bee Configuration
;;;

;; The field names here deliberately mirror the bee config entry names letter
;; by letter.
(define-configuration bee-configuration
  (debug-api-enable      (boolean #false) "")
  (db-open-files-limit   (non-negative-integer +db-open-files-limit/default+) "")
  (verbosity             (verbosity-value 'info)
                         "Either an integer between 0 and 5, or one of the \
symbols: @code{'silent}, @code{'error}, @code{'warn}, @code{'info}, \
@code{'debug}, @code{'trace}.")
  (full-node             (boolean #true) "")
  (p2p-addr              (string ":1634") "")
  (api-addr              (string ":1633") "")
  (debug-api-addr        (string "localhost:1635") "")
  (resolver-options      (string "")
                         "A blockchain node endpoint to connect to for resolving ENS names.
Normally it should be a node connected to the Ethereum mainnet.")
  (blockchain-rpc-endpoint         string "A blockchain node endpoint to connect to.")
  (swap-enable           maybe-boolean "")
  (swap-initial-deposit  maybe-non-negative-integer "")

  (mainnet               (boolean #true) "")
  (network-id            (non-negative-integer 1) "")
  (password-file         maybe-string "")
  (data-dir              maybe-string "")
  (extra-config          raw-configuration-string
                         "A string that will be appended as-is to the end of the generated \
@code{bee.yml} files."))

;;;
;;; Service Configuration
;;;
(define-configuration/no-serialization swarm-service-configuration
  (swarm                 (swarm swarm/mainnet)
   "An instance of <swarm> that describes the parameters of the swarm the \
nodes should join.  Defaults to swarm/mainnet.")
  (bee-configuration     bee-configuration "")
  ;; Packages
  (bee                   (file-like bee-binary)
                         "The Bee Guix package to use.")
  (node-count            (non-negative-integer 1)
   "How many Bee nodes should be started.")
  (shepherd-requirement
   (list '())
   "Guix service names that are appended to the REQUIREMENT field of each \
Bee node's Shepherd service instance; i.e. here you can specify extra \
dependencies for the start order of the services, e.g. if you are running \
a local Gnosis chain node instance, then you can add its name here.")
  ;; Users and groups
  (bee-user              maybe-user-account
                         "USER-ACCOUNT object specifying the Unix user for the Bee processes.")
  (swarm-group           maybe-user-group
                         "Unix group for the users in this swarm. Defaults to a system group with name 'swarm-[swarm-name]'"))

(define (apply-config-defaults service-config)
  (match-record service-config <swarm-service-configuration>
    (swarm bee-user swarm-group
           (bee-configuration bee-cfg))
    (match-record bee-cfg <bee-configuration>
        (mainnet data-dir password-file (network-id bee/network-id))
      (match-record swarm <swarm>
          ((name swarm-name) (network-id swarm/network-id))
        (let ((swarm-group (maybe-value
                            swarm-group
                            (user-group
                             (name (string-append "swarm-" swarm-name))
                             (system? #t)))))
          ;; Return with a copy in which everything is fully specified.
          (swarm-service-configuration
           (inherit service-config)
           (swarm-group swarm-group)
           (bee-user  (maybe-value
                       bee-user
                       (user-account
                        (name (string-append "bee-" swarm-name))
                        (group (user-group-name swarm-group))
                        (supplementary-groups '("swarm"))
                        (system? #t)
                        (comment (string-append "Swarm Bee service user for swarm " swarm-name))
                        (home-directory (swarm-data-directory swarm-name))
                        (shell (file-append shadow "/sbin/nologin")))))
           (bee-configuration
            ;; This config will be used when NODE-COUNT is 1. Otherwise
            ;; BEE-CONFIGURATION-FOR-NODE-INDEX will be used to generate the
            ;; indexed cfg instances for each node.
            (bee-configuration
             (inherit bee-cfg)
             (mainnet              (maybe-value mainnet        (equal? swarm-name "mainnet")))
             (network-id           (maybe-value bee/network-id swarm/network-id))
             (password-file        (maybe-value password-file  (bee-password-file swarm-name 0)))
             (data-dir             (maybe-value data-dir       (bee-data-directory swarm-name 0)))))))))))

(define (bee-configuration-for-node-index swarm template node-index)

  (define (as-addr-string port-base)
    (string-append ":" (number->string (+ port-base node-index))))

  (match-record swarm <swarm>
      (p2p-port-base api-port-base debug-api-port-base
                     (name swarm-name))
    (bee-configuration
     (inherit template)
     (p2p-addr       (as-addr-string p2p-port-base))
     (api-addr       (as-addr-string api-port-base))
     (debug-api-addr (string-append "localhost" (as-addr-string debug-api-port-base)))
     (data-dir       (bee-data-directory swarm-name node-index))
     (password-file  (bee-password-file  swarm-name node-index)))))

;;;
;;; Service implementation
;;;
(define (make-shepherd-service/bee bee-index service-config)
  (with-service-gexp-modules '((guix build utils)
                               (guix-crypto swarm-utils))
   (match-record service-config <swarm-service-configuration>
       (swarm bee-configuration bee bee-user swarm-group node-count
              shepherd-requirement)
     (match-record swarm <swarm>
         ((name swarm-name) network-id)
       (match-record bee-configuration <bee-configuration>
           (full-node resolver-options blockchain-rpc-endpoint
                      db-open-files-limit password-file)
         ;; TODO add: cashout, withdraw, balances, settlements, backup-identity
         (let* ((data-dir     (bee-data-directory swarm-name bee-index))
                (bee-cfg      (if (equal? 1 node-count)
                                  bee-configuration
                                  (bee-configuration-for-node-index
                                   swarm bee-configuration bee-index)))
                (config-file  (plain-file
                               (simple-format #f "bee-config-~A.yaml" bee-index)
                               (with-output-to-string
                                 (lambda ()
                                   (serialize-configuration
                                    bee-cfg
                                    bee-configuration-fields))))))

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
              #~(let ((filename #$(bee-wallet-file swarm-name bee-index)))
                  (display (wallet-file-address filename)))))

           (define log-file-action
             (shepherd-action/bee
              'log-file
              "Print the Bee node's log file path."
              #~(display #$(bee-log-filename (default-log-directory swarm-name)
                                             bee-index))))

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
                    (let* ((data-dir (bee-data-directory #$swarm-name #$bee-index))
                           (dest-dir (canonicalize-path (first -args-)))
                           (file-name (string-append (date->string (current-date) "~1") "-"
                                                     (gethostname)
                                                     "-bee-"
                                                     #$swarm-name "-"
                                                     #$(number->string bee-index)))
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
                                          "install --mode=600 <("
                                          (string-join cmd " ")
                                          " | " compressor ") " dest-path)))
                        (log.debug "Will run now: ls -l /bin/sh") ; TODO delme
                        (system "ls -l /bin/sh") ; TODO delme
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
            (documentation (simple-format #f "Swarm bee node ~S in swarm ~S."
                                          bee-index swarm-name))
            (provision (list (bee-service-name swarm-name bee-index)))
            (requirement (append '(networking file-systems)
                                 shepherd-requirement))
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
                    #~(let ((swarm-name '#$swarm-name)
                            (bee-index  '#$bee-index)
                            (data-dir   '#$data-dir)
                            (password-file '#$password-file))
                        (log.debug "Bee service is starting")

                        ;; KLUDGE TODO this is here because shepherd respawns us in a busy loop
                        ((@ (fibers) sleep) 2)

                        (ensure-directories/rec bee-user-id bee-group-id #o2770 data-dir)
                        (ensure-password-file password-file bee-user-id bee-group-id)

                        (let ()
                          (define (spawn-bee* action)
                            (let ((cmd (list #$(file-append bee "/bin/bee")
                                             "--config" #$config-file
                                             action)))
                              (log.dribble "About to spawn Bee, cmd is: ~S" cmd)
                              (fork+exec-command
                               cmd
                               #:user '#$(user-account-name bee-user)
                               #:group '#$(user-group-name swarm-group)
                               #:log-file (bee-log-filename (default-log-directory swarm-name)
                                                            bee-index)
                               #:directory data-dir
                               #:resource-limits `((nofile ,#$(+ db-open-files-limit 4096)
                                                           ,#$(+ db-open-files-limit 4096)))
                               #:environment-variables
                               (delete #false
                                       (append
                                        (list
                                         (string-append "HOME=" data-dir)
                                         ;; So that these are not visible with ps, or in the
                                         ;; config file (i.e. world-readable under
                                         ;; /gnu/store/), because they may contain keys when
                                         ;; using a service like Infura.
                                         (and #$blockchain-rpc-endpoint
                                              (string-append "BEE_BLOCKCHAIN_RPC_ENDPOINT="
                                                             #$blockchain-rpc-endpoint))
                                         (and #$resolver-options
                                              (string-append "BEE_RESOLVER_OPTIONS=" #$resolver-options)))
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
                            (log.debug "Invoking `bee init` for bee-index ~S in swarm ~S" bee-index swarm-name)
                            (wait-for-pid
                             (spawn-bee* "init"))
                            (log.dribble "`bee init` finished for bee-index ~S in swarm ~S" bee-index swarm-name))

                          ;; Due to staged compilation, we cannot add the node's
                          ;; eth address to the config bee file, because it only
                          ;; gets generated at service runtime. Hence we're
                          ;; passing it as an env variable.
                          (spawn-bee* "start"))))))
            (stop #~(make-kill-destructor #:grace-period 120)))))))))

(define (make-swarm-shepherd-services service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (node-count)
    (map (lambda (bee-index)
           (make-shepherd-service/bee bee-index service-config))
         (iota node-count))))

(define (swarm-service-start-gexp service-config body-gexp)
  "Returns a GEXP that is called before the start of any of the services.  It
means that this GEXP should only do operations that are safe to be called any
number of times, in any random moment."
  (match-record service-config <swarm-service-configuration>
      (swarm bee-configuration node-count bee-user swarm-group)
    (match-record bee-configuration <bee-configuration>
        (mainnet)
      (match-record swarm <swarm>
          ((name swarm-name) network-id)
        ;; TODO use the -foo- naming convention for the implicit bindings below?
        ;; TODO simplify this, and maybe split into two for the actions
        #~(let* ((swarm-name    #$swarm-name)
                 (path          #$(file-append coreutils "/bin"))
                 (bash          #$(file-append bash-minimal "/bin/bash"))
                 (tr            #$(file-append coreutils "/bin/tr"))
                 (head          #$(file-append coreutils "/bin/head"))
                 (chown-bin     #$(file-append coreutils "/bin/chown"))
                 ;; reduce/rebind the USER-ACCOUNT and USER-GROUP instances into their names
                 (swarm-group   #$(user-group-name swarm-group))
                 (bee-user      #$(user-account-name bee-user))
                 (bee-pw        (getpwnam bee-user))
                 (bee-user-id   (passwd:uid bee-pw))
                 (bee-group-id  (passwd:gid bee-pw))
                 (log-dir       #$(default-log-directory swarm-name)))

            ;; so that we can already invoke basic commands before the fork+exec
            (setenv "PATH" path)
            (with-log-directory log-dir
                                ;; Log files are not visible to everyone.
              (ensure-directories/rec bee-user swarm-group #o2770
                                      log-dir)

              ;; Ensure as root that the service.log file exists, and it is
              ;; group writable.
              (let ((path (service-log-filename)))
                (close-port (open-file path "a"))
                (chmod path #o664))

              (log.dribble "A SWARM-SERVICE-START-GEXP is running for swarm ~S on Guile version ~S" swarm-name (version))

              (let ((dir #$(swarm-data-directory swarm-name)))
                ;; The data dir is visible to everyone.
                (ensure-directories 0 swarm-group #o2770 dir)
                (log.debug "Ensured directory ~S" dir))

              #$body-gexp))))))

;; TODO factor out? this is a stripped down version of the above.
(define (swarm-service-action-gexp service-config body-gexp)
  (match-record service-config <swarm-service-configuration>
      (swarm bee-user swarm-group)
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
                (ensure-directories/rec #$(user-account-name bee-user)
                                        #$(user-group-name swarm-group)
                                        #o2770
                                        -log-dir-)
                #$body-gexp)))))))

(define (make-swarm-user-accounts service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (swarm bee-user swarm-group bee-configuration)
    (match-record swarm <swarm>
        ((name swarm-name))
      (list (user-group
             (name "swarm")
             (system? #t))
            swarm-group
            bee-user))))

(define (make-swarm-log-rotations service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (swarm node-count bee-configuration)
    (match-record swarm <swarm>
        ((name swarm-name))
      (let ((log-dir (default-log-directory swarm-name)))
        (list
         (log-rotation
          (files (cons*
                  (string-append log-dir "/service.log")
                  (map (cut bee-log-filename log-dir <>)
                       (iota node-count))))
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
