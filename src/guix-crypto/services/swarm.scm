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
            swarm-service))

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
  (clef-chain-id         non-negative-integer "")
  (p2p-port-base         non-negative-integer
   "Base number for the p2p ports of the Bee nodes.  Defaults to 1600 for \
@code{mainnet} and 1900 for @code{testnet}, otherwise it must be specified.")
  (api-port-base         non-negative-integer
   "Base number for the api ports of the Bee nodes.  Defaults to 1700 for \
@code{mainnet} and 2000 for @code{testnet}, otherwise it must be specified.")
  (debug-api-port-base   non-negative-integer
   "Base number for the debug api ports of the Bee nodes. Defaults to 1800 \
for @code{mainnet} and 2100 for @code{testnet}, otherwise it must be specified."))

(define-public swarm/mainnet
  (swarm
   (name                "mainnet")
   (network-id          1)
   (clef-chain-id       100)
   (p2p-port-base       1600)
   (api-port-base       1700)
   (debug-api-port-base 1800)))

(define-public swarm/testnet
  (swarm
   (name                "testnet")
   (network-id          10)
   (clef-chain-id       5)
   (p2p-port-base       11600)
   (api-port-base       11700)
   (debug-api-port-base 11800)))

;;;
;;; Bee Configuration
;;;

;; The field names here deliberately mirror the bee config entry names letter
;; by letter.
(define-configuration bee-configuration
  (clef-signer-enable    (boolean #false)
                         "Whether to connect to a Clef instance, or the Bee node itself should be doing \
its own Ethereum key management. Disabled by default because the Swarm team
does not recommend using this setup anymore.")
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
  (swap-initial-deposit  maybe-non-negative-integer "")

  (mainnet               (boolean #true) "")
  (network-id            (non-negative-integer 1) "")
  (password-file         maybe-string "")
  (data-dir              maybe-string "")
  (clef-signer-endpoint  maybe-string "")
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
  (geth                  (file-like geth-binary)
                         "The go-ethereum (geth) package to use (for the \
@code{clef} binary).")
  (node-count            (non-negative-integer 1)
   "How many Bee nodes should be started.")
  (shepherd-requirement
   (list '())
   "Guix service names that are appended to the REQUIREMENT field of each \
Bee node's Shepherd service instance; i.e. here you can specify extra \
dependencies for the start order of the services, e.g. if you are running \
a local Gnosis chain node instance, then you can add its name here.")
  ;; Users and groups
  (bee-user              maybe-string
                         "Unix user for the Bee processes in this swarm.")
  (bee-user-id           maybe-non-negative-integer
                         "Unix uid for @code{bee-user}.")
  (bee-supplementary-groups (list '())
                            "Supplementary groups for the BEE-USER.")
  (clef-user             maybe-string
                         "Unix user for the clef process in this swarm.")
  (clef-user-id          maybe-non-negative-integer
                         "Unix uid for @code{clef-user}.")
  (swarm-group           maybe-string
                         "Unix group for the users in this swarm.")
  (swarm-group-id        maybe-non-negative-integer
                         "Unix gid for @code{swarm-group}."))

(define (apply-config-defaults service-config)
  (match-record service-config <swarm-service-configuration>
    (swarm bee-user clef-user swarm-group
           (bee-configuration bee-cfg))
    (match-record bee-cfg <bee-configuration>
        (mainnet data-dir password-file (network-id bee/network-id)
        clef-signer-enable clef-signer-endpoint)
      (match-record swarm <swarm>
          ((name swarm-name) (network-id swarm/network-id))
        ;; Return with a copy in which everything is fully specified.
        (swarm-service-configuration
         (inherit service-config)
         (bee-user       (maybe-value bee-user    (string-append "bee-"   swarm-name)))
         (clef-user      (maybe-value clef-user   (string-append "clef-"  swarm-name)))
         (swarm-group    (maybe-value swarm-group (string-append "swarm-" swarm-name)))
         (bee-configuration
          ;; This config will be used when NODE-COUNT is 1. Otherwise
          ;; BEE-CONFIGURATION-FOR-NODE-INDEX will be used to generate the
          ;; indexed cfg instances for each node.
          (bee-configuration
           (inherit bee-cfg)
           (mainnet              (maybe-value mainnet        (equal? swarm-name "mainnet")))
           (network-id           (maybe-value bee/network-id swarm/network-id))
           (password-file        (maybe-value password-file  (bee-password-file swarm-name)))
           (data-dir             (maybe-value data-dir       (bee-data-directory swarm-name 0)))
           (clef-signer-endpoint (maybe-value clef-signer-endpoint
                                              (if clef-signer-enable
                                                  (clef-ipc-file swarm-name)
                                                  %unset-value))))))))))

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
     (data-dir       (bee-data-directory swarm-name node-index)))))

;;;
;;; Service implementation
;;;
(define (make-shepherd-service/clef service-config)
  (with-service-gexp-modules '((guix-crypto swarm-utils))
    (match-record service-config <swarm-service-configuration>
        (swarm geth clef-user swarm-group)
      (match-record swarm <swarm>
          ((name swarm-name) clef-chain-id)
        (shepherd-service
         (documentation (simple-format #f "Swarm clef instance in swarm ~S."
                                       swarm-name))
         (provision (list (clef-service-name swarm-name)))
         (requirement '(networking file-systems))
         (modules (append
                   '((guix-crypto swarm-utils))
                   +default-service-modules+))
         (start
          (let* ((data-dir     (clef-data-directory     swarm-name))
                 (keystore-dir (clef-keystore-directory swarm-name))
                 (start-script (local-file "swarm-clef-start-script"))
                 (cmd #~(list #$(file-append bash-minimal "/bin/bash")
                              #$start-script
                              (string-append #$geth:clef "/bin/clef")
                              #$data-dir #$keystore-dir
                              #$(number->string clef-chain-id)
                              #$(upstream-bee-clef-file "/packaging/4byte.json"))))
            #~(lambda args
                #$(swarm-service-start-gexp
                   service-config
                   #~(begin
                       (log.debug "Clef service is starting")

                       #$(clef-activation-gexp service-config)

                       (log.debug "Will launch clef using cmd ~A" #$cmd)

                       (let* ((clef-password (read-file-to-string
                                              #$(clef-password-file swarm-name)))
                              (clef-stdout #$(string-append data-dir "/stdout"))
                              (clef-stdin  #$(string-append data-dir "/stdin")))

                         (false-if-exception (delete-file clef-stdout))
                         (false-if-exception (delete-file clef-stdin))

                         (let ((pid (fork+exec-command
                                     #$cmd
                                     #:user #$clef-user
                                     #:group #$swarm-group
                                     #:log-file (string-append (*log-directory*) "/clef.log")
                                     #:environment-variables
                                     (append
                                      (list
                                       (string-append "HOME=" #$data-dir)
                                       (string-append "PATH=" path)
                                       (string-append "CLEF_PASSWORD=" clef-password))
                                      +root-environment+))))

                           ((@ (fibers) spawn-fiber)
                            (lambda ()
                              (let ((wait-some (lambda ()
                                                 ((@ (fibers) sleep) 0.3))))
                                (log.debug "Clef fiber is speaking, clef pid is ~S" pid)
                                (let loop ()
                                  (unless (is-pid-alive? pid)
                                    (wait-some)
                                    (loop)))
                                (log.debug "clef process is available according to (is-pid-alive? pid)")
                                (let loop ()
                                  (unless (file-exists? clef-stdout)
                                    (wait-some)
                                    (loop)))
                                (log.debug "File ~S showed up, about to call CLEF-STDIO-LOOP" clef-stdout)

                                ;; TODO this doesn't work. at some point we get a file closed error on the input stream
                                ;; possibly due to how fiber does the non-local returns/continuations?
                                ;; (call-with-input-file clef-stdout
                                ;;   (lambda (input)
                                ;;     (call-with-output-file clef-stdin
                                ;;       (lambda (output)
                                ;;         (spawn-clef-stdio-fiber pid input output
                                ;;                          clef-password)))))

                                ;; TODO this open/close structure is not safe for interim errors and whatnot
                                ;; TODO who will close these files and when?
                                (let ((input  (open-file clef-stdout "r"))
                                      (output (open-file clef-stdin  "w")))
                                  (make-port-non-blocking! input)
                                  (make-port-non-blocking! output)

                                  (spawn-clef-stdio-fiber pid input output
                                                          clef-password)))))

                           ;; We need to do this here, because we must not return
                           ;; from START until Clef is properly up and running,
                           ;; otherwise the (REQUIREMENT `(CLEF-SERVICE-NAME)) is
                           ;; useless on the bee service.
                           (ensure-ipc-file-permissions pid #$(clef-ipc-file swarm-name))
                           pid)))))))
         (stop
          (let* ((data-dir (clef-data-directory swarm-name)))
            #~(lambda (pid . args)
                (apply (make-kill-destructor #:grace-period 120) pid args)
                ;; Just some best effort cleanup that is not necessary.
                (false-if-exception (delete-file #$(string-append data-dir "/clef.ipc")))
                (false-if-exception (delete-file #$(string-append data-dir "/stdin")))
                (false-if-exception (delete-file #$(string-append data-dir "/stdout")))
                #false))))))))

(define (make-shepherd-service/bee bee-index service-config singular?)
  (with-service-gexp-modules '((guix build utils)
                               (guix-crypto swarm-utils)
                               (json))
   (match-record service-config <swarm-service-configuration>
       (swarm bee-configuration bee bee-user swarm-group node-count
              shepherd-requirement)
     (match-record swarm <swarm>
         ((name swarm-name) network-id)
       (match-record bee-configuration <bee-configuration>
           (full-node resolver-options blockchain-rpc-endpoint clef-signer-enable
                      db-open-files-limit)
         ;; TODO add: cashout, withdraw, balances, settlements, backup-identity
         (let* ((data-dir     (bee-data-directory swarm-name bee-index))
                (libp2p-key   (string-append data-dir "/keys/libp2p.key"))
                (bee-cfg      (if singular?
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
              (if clef-signer-enable
                  #~(display (ensure-clef-account #$swarm-name #$bee-index))
                  ;; TODO maybe we should use an RPC call instead? but that
                  ;; would only work when the node is running.
                  #~(let* ((filename #$(bee-wallet-file swarm-name bee-index))
                           (json (call-with-input-file filename
                                   json->scm))
                           (address (assoc-ref json "address")))
                      (display address)))))

           (define log-file-action
             (shepherd-action/bee
              'log-file
              "Print the Bee node's log file path."
              #~(display #$(bee-log-filename (default-log-directory swarm-name)
                                             bee-index))))

           (define backup-identity-action
             (shepherd-action
              (name 'backup-identity)
              (documentation "Backs up the Bee node's identity (the statestore,
its wallet when running without clef, and the libp2p and pss keys) into the
directory specified as the first command line argument.")
              (procedure
               (swarm-service-action-gexp
                service-config
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
                        (let ((cmd (cons* #$(file-append tar "/bin/tar")
                                          "--gzip" "--verbose" "--create"
                                          "--file" dest-path
                                          "--directory" data-dir
                                          "keys/"
                                          "statestore/"
                                          (if #$clef-signer-enable
                                              (error "TODO backing up the Clef key is not yet implemented")
                                              '()))))
                          (log.debug "Will run backup cmd: ~S" cmd)
                          (apply system* cmd))
                        (format #t "Node identity files have been backed up into '~A'~%"
                                dest-path)))))
                   (else
                    (format (current-error-port) "Usage: herd backup-identity bee-mainnet-0 destination-directory.~%")))))))

           (shepherd-service
            (documentation (simple-format #f "Swarm bee node ~S in swarm ~S."
                                          bee-index swarm-name))
            (provision (list (bee-service-name swarm-name bee-index)))
            (requirement (append '(networking file-systems)
                                 (if clef-signer-enable
                                     (list (clef-service-name swarm-name))
                                     '())
                                 shepherd-requirement))
            (actions (list address-action
                           log-file-action
                           (shepherd-configuration-action config-file)
                           backup-identity-action))
            (modules (append
                      '((guix-crypto swarm-utils)
                        (json))
                      +default-service-modules+))
            (start
             #~(lambda _
                 #$(swarm-service-start-gexp
                    service-config
                    #~(begin
                        (log.debug "Bee service is starting (~A Clef)" (if #$clef-signer-enable "with" "without"))

                        ;; KLUDGE TODO this is here because shepherd respawns us in a busy loop
                        ((@ (fibers) sleep) 2)

                        (ensure-directories/rec bee-user-id bee-group-id #o2770 #$data-dir)
                        (ensure-password-file #$(bee-password-file swarm-name) bee-user-id bee-group-id)

                        (let ((eth-address (and #$clef-signer-enable
                                                (ensure-clef-account #$swarm-name #$bee-index))))

                          (define (spawn-bee* action)
                            (spawn-bee #$(file-append bee "/bin/bee")
                                       #$config-file
                                       action
                                       #$swarm-name #$bee-index
                                       #$bee-user #$swarm-group
                                       #:blockchain-rpc-endpoint #$blockchain-rpc-endpoint
                                       #:resolver-options #$resolver-options
                                       #:eth-address eth-address
                                       #:resource-limits
                                       `((nofile ,#$(+ db-open-files-limit 4096)
                                                 ,#$(+ db-open-files-limit 4096)))))

                          ;; When first started, call `bee init` for this bee
                          ;; instance.
                          (unless (file-exists? #$libp2p-key)
                            (log.debug "Invoking `bee init` for bee-index ~S in swarm ~S" #$bee-index #$swarm-name)
                            (wait-for-pid
                             (spawn-bee* "init"))
                            (log.dribble "`bee init` finished for bee-index ~S in swarm ~S" #$bee-index #$swarm-name))

                          ;; Due to staged compilation, we cannot add the node's
                          ;; eth address to the config bee file, because it only
                          ;; gets generated at service runtime. Hence we're
                          ;; passing it as an env variable.
                          (spawn-bee* "start"))))))
            (stop #~(make-kill-destructor #:grace-period 120)))))))))

(define (make-swarm-shepherd-services service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (node-count bee-configuration)
    (match-record bee-configuration <bee-configuration>
        (clef-signer-enable)
      (append
       (if clef-signer-enable
           (list (make-shepherd-service/clef service-config))
           '())
       (map (lambda (bee-index)
              (make-shepherd-service/bee bee-index service-config
                                         (equal? 1 node-count)))
            (iota node-count))))))

(define (upstream-bee-clef-file relative-path)
  (let ((bee-clef-git
         (let ((commit "v0.13.2"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ethersphere/bee-clef.git")
                   (commit commit)))
             (file-name (git-file-name "bee-clef" commit))
             (sha256
              (base32
               "02hrsykn47rp5zhm5dic76pxi5618b5p3qyz8mkxwf0d9s5bnr17"))))))
    (file-append bee-clef-git relative-path)))

(define (clef-activation-gexp service-config)
  (match-record service-config <swarm-service-configuration>
      (swarm geth node-count bee-user clef-user swarm-group)
    (match-record swarm <swarm>
        ((name swarm-name))
      (let ((data-dir      (clef-data-directory swarm-name))
            (keystore-dir  (clef-keystore-directory swarm-name)))
        #~(let* ((clef-pwd-file #$(clef-password-file swarm-name))
                 (clef-pw       (getpwnam #$clef-user))
                 (clef-user-id  (passwd:uid clef-pw))
                 (clef-group-id (passwd:gid clef-pw)))

            (log.debug "Clef activation started")

            (define (build-clef-cmd . args)
              (apply string-append
                     #$geth:clef "/bin/clef \\\n"
                     "--configdir \"" #$data-dir "\" \\\n"
                     "--keystore \"" #$keystore-dir "\" \\\n"
                     "--stdio-ui \\\n"
                     args))

            (define (invoke-clef-cmd cmd)
              (log.debug "Will invoke clef cmd: ~A" cmd)
              (invoke bash "-c" cmd))

            (ensure-directories 0 "swarm" #o2775
                                #$*service-log-directory*
                                #$*service-data-directory*)

            (ensure-directories/rec clef-user-id clef-group-id #o750
                                    #$data-dir)

            (ensure-directories/rec clef-user-id clef-group-id #o700
                                    #$(clef-keystore-directory swarm-name))

            (ensure-password-file clef-pwd-file clef-user-id clef-group-id)

            (log.debug "Clef activation ensured the password and the keystore")

            (invoke-as-user
             clef-pw
             (lambda ()
               (log.debug "Initializing clef for swarm ~S" #$swarm-name)
               ;; Sending the password in the command line would expose it.
               (setenv "CLEF_PASSWORD" (read-file-to-string clef-pwd-file))
               (setenv "PATH" #$(file-append coreutils "/bin"))
               (let ((master-seed (string-append #$data-dir "/masterseed.json")))
                 ;; Ensure that clef is initialized
                 (unless (file-exists? master-seed)
                   (log.debug "Generating clef master seed at ~S for swarm ~S" master-seed #$swarm-name)
                   (invoke-clef-cmd (build-clef-cmd "init >/dev/null 2>&1 << EOF
$CLEF_PASSWORD
$CLEF_PASSWORD
EOF"))
                   )))))))))

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
                 (bee-pw        (getpwnam #$bee-user))
                 (bee-user-id   (passwd:uid bee-pw))
                 (bee-group-id  (passwd:gid bee-pw))
                 (log-dir       #$(default-log-directory swarm-name)))

            ;; so that we can already invoke basic commands before the fork+exec
            (setenv "PATH" path)
            (with-log-directory log-dir
                                ;; Log files are not visible to everyone.
              (ensure-directories/rec #$bee-user #$swarm-group #o2770
                                      log-dir)

              ;; Ensure as root that the service.log file exists, and it is group
              ;; writable (because both the bee and the clef service code logs into
              ;; it).
              (let ((path (service-log-filename)))
                (close-port (open-file path "a"))
                (chmod path #o664))

              (log.dribble "A SWARM-SERVICE-START-GEXP is running for swarm ~S on Guile version ~S" swarm-name (version))

              (let ((dir #$(swarm-data-directory swarm-name)))
                ;; The data dir is visible to everyone.
                (ensure-directories 0 #$swarm-group #o2770 dir)
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
            (let* ((-path-          #$(file-append coreutils "/bin"))
                   (-log-dir-       #$(default-log-directory swarm-name)))
              (setenv "PATH" -path-)
              (with-log-directory -log-dir-
                                  ;; Log files are not visible to everyone.
                                  (ensure-directories/rec #$bee-user #$swarm-group #o2770
                                                          -log-dir-)
                                  #$body-gexp)))))))

(define (make-swarm-user-accounts service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <swarm-service-configuration>
      (swarm bee-user bee-user-id clef-user clef-user-id
             swarm-group swarm-group-id bee-supplementary-groups
             bee-configuration)
    ;; NOTE it's safe to forward the #false default value of uid/gid to
    ;; USER-ACCOUNT.
    (match-record swarm <swarm>
        ((name swarm-name))
      (match-record bee-configuration <bee-configuration>
          (clef-signer-enable)
        (append
         (if clef-signer-enable
             (list
              (user-account
               (name clef-user)
               (uid (maybe-value clef-user-id))
               (group swarm-group)
               (system? #t)
               (comment (string-append "Service user for the clef instance of swarm "
                                       swarm-name))
               (home-directory (swarm-data-directory swarm-name))
               (shell (file-append shadow "/sbin/nologin"))))
             '())
         (list (user-group
                (name "swarm")
                (system? #t))
               (user-group
                (name swarm-group)
                (id (maybe-value swarm-group-id))
                (system? #t))
               (user-account
                (name bee-user)
                (uid (maybe-value bee-user-id))
                (group swarm-group)
                (supplementary-groups bee-supplementary-groups)
                (system? #t)
                (comment (string-append "Swarm Bee service user for swarm "
                                        swarm-name))
                (home-directory (swarm-data-directory swarm-name))
                (shell (file-append shadow "/sbin/nologin")))))))))

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
                  (string-append log-dir "/clef.log")
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
   (description "Runs the requested number of Bee instances, and optionally an \
Ethereum Clef instance as a group of Shepherd services.")))
