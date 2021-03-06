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

(define-module (guix-crypto services swarm)
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto service-utils)
  #:use-module (guix-crypto packages swarm)
  #:use-module (guix-crypto packages ethereum)
  #:use-module (guix diagnostics)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module ((guix records) #:hide (match-record)) ;; TODO temporarily
  #:use-module (guix packages)
  #:use-module (guix git-download)
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
  (display value))

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
  (clef-signer-enable    (boolean #true)
                         "Whether to connect to a clef instance, or the Bee node should be doing \
its own Ethereum key management.")
  (debug-api-enable      (boolean #false) "")
  (db-open-files-limit   (non-negative-integer +db-open-files-limit/default+) "")
  (global-pinning-enable (boolean #false) "")
  (verbosity             (verbosity-value 'info)
                         "Either an integer between 0 and 5, or one of the \
symbols: @code{'silent}, @code{'error}, @code{'warn}, @code{'info}, \
@code{'debug}, @code{'trace}.")
  (full-node             (boolean #true) "")
  (p2p-addr              (string ":1634") "")
  (api-addr              (string ":1633") "")
  (debug-api-addr        (string ":1635") "")
  (resolver-options      (string "")
                         "A blockchain node endpoint to connect to for resolving ENS names.
Normally it should be a node connected to the Ethereum mainnet.")
  (swap-endpoint         string "A blockchain node endpoint to connect to.")
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
  ;; TODO rename to shepherd-requirement
  (additional-service-requirements
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
        (mainnet data-dir password-file (network-id bee/network-id))
      (match-record swarm <swarm>
          ((name swarm-name) (network-id swarm/network-id))
        ;; Return with a copy in which everything is fully specified.
        (swarm-service-configuration
         (inherit service-config)
         (bee-user       (or (defined-value? bee-user)
                             (string-append "bee-"   swarm-name)))
         (clef-user      (or (defined-value? clef-user)
                             (string-append "clef-"  swarm-name)))
         (swarm-group    (or (defined-value? swarm-group)
                             (string-append "swarm-" swarm-name)))
         (bee-configuration
          ;; This config will be used when NODE-COUNT is 1. Otherwise
          ;; BEE-CONFIGURATION-FOR-NODE-INDEX will be used to generate the
          ;; indexed cfg instances for each node.
          (bee-configuration
           (inherit bee-cfg)
           (mainnet              (or (defined-value? mainnet)
                                     (equal? swarm-name "mainnet")))
           (network-id           (or (defined-value? bee/network-id)
                                     swarm/network-id))
           (password-file        (or (defined-value? password-file)
                                     (bee-password-file swarm-name)))
           (data-dir             (or (defined-value? data-dir)
                                     (bee-data-directory swarm-name 0)))
           (clef-signer-endpoint (clef-ipc-file swarm-name)))))))))

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
     (debug-api-addr (as-addr-string debug-api-port-base))
     (data-dir       (bee-data-directory swarm-name node-index)))))

;;;
;;; Service implementation
;;;
(define +service-log-directory+ "/var/log/swarm/")

(define-public (default-log-directory swarm-name)
  (string-append +service-log-directory+ swarm-name))

(define-public (bee-log-filename log-dir bee-index)
  (simple-format #f "~A/bee-~A.log" log-dir bee-index))

(define +service-data-directory+ "/var/lib/swarm/")

(define (swarm-data-directory swarm-name)
  (string-append +service-data-directory+ swarm-name))

(define-public (bee-data-directory swarm-name bee-index)
  (string-append (swarm-data-directory swarm-name) "/bee-"
                 (number->string bee-index)))

(define-public (bee-account-file swarm-name bee-index)
  (string-append (bee-data-directory swarm-name bee-index) "/eth-address"))

(define-public (clef-data-directory swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef"))

(define-public (clef-ipc-file swarm-name)
  (string-append (clef-data-directory swarm-name) "/clef.ipc"))

(define-public (clef-keystore-directory swarm-name)
  (string-append (clef-data-directory swarm-name) "/keystore"))

(define-public (bee-password-file swarm-name)
  (string-append (swarm-data-directory swarm-name) "/bee-password"))

(define-public (clef-password-file swarm-name)
  (string-append (swarm-data-directory swarm-name) "/clef-password"))

(define-public (clef-service-name swarm-name)
  (string->symbol (simple-format #f "clef-~A" swarm-name)))

(define-public (bee-service-name swarm-name bee-index)
  (string->symbol (simple-format #f "bee-~A-~A" swarm-name bee-index)))

(define (make-shepherd-service/clef service-config)
  (with-service-gexp-modules '()
    (match-record service-config <swarm-service-configuration>
        (swarm geth clef-user swarm-group)
      (match-record swarm <swarm>
          ((name swarm-name) clef-chain-id)
        (shepherd-service
         (documentation (simple-format #f "Swarm clef instance in swarm ~S."
                                       swarm-name))
         (provision (list (clef-service-name swarm-name)))
         (requirement '(networking file-systems))
         (modules +default-service-modules+)
         (start
          (let* ((data-dir     (clef-data-directory     swarm-name))
                 (keystore-dir (clef-keystore-directory swarm-name))
                 (start-script (local-file "swarm-clef-start-script"))
                 (cmd #~(list #$(file-append bash-minimal "/bin/bash")
                              #$start-script
                              (string-append #$geth:clef "/bin/clef")
                              #$data-dir #$keystore-dir
                              #$(number->string clef-chain-id)
                              #$(rules.js)
                              #$(upstream-bee-clef-file "/packaging/4byte.json"))))
            ;; TODO it would be nice to get rid of the start shell script, but
            ;; i don't want to get into pipes and stuff in scheme when
            ;; upstream has already put together a shell script.
            #~(lambda args
                #$(swarm-service-gexp
                   service-config
                   #~(begin
                       (log.debug "Clef service is starting")

                       #$(clef-activation-gexp service-config)

                       (define forkexec
                         (make-forkexec-constructor
                          #$cmd
                          #:user #$clef-user
                          #:group #$swarm-group
                          #:log-file (string-append (*log-directory*) "/clef.log")
                          #:environment-variables
                          (list (string-append "HOME=" #$data-dir)
                                (string-append "PATH=" path)
                                (string-append "CLEF_PASSWORD="
                                               (read-file-to-string
                                                #$(clef-password-file swarm-name)))
                                "LC_ALL=en_US.UTF-8")))

                       ;; We need to do this here, because we must not return
                       ;; from start until clef is properly up and running,
                       ;; otherwise the (requirement `(clef-service-name)) is
                       ;; useless on the bee service.
                       (let ((pid (apply forkexec args)))
                         (ensure-ipc-file-permissions pid #$(clef-ipc-file swarm-name))
                         pid))))))
         (stop #~(make-kill-destructor)))))))

(define (make-shepherd-service/bee bee-index service-config singular?)
  (with-service-gexp-modules '()
   (match-record service-config <swarm-service-configuration>
       (swarm bee-configuration bee bee-user swarm-group node-count
              additional-service-requirements)
     (match-record swarm <swarm>
         ((name swarm-name) network-id)
       (match-record bee-configuration <bee-configuration>
           (full-node resolver-options swap-endpoint clef-signer-enable
                      db-open-files-limit)

         (define display-address-action
            (shepherd-action
             (name 'display-address)
             (documentation "Print the Bee node's Ethereum address.")
             (procedure
              #~(lambda _
                  (let* ((acc-file #$(bee-account-file swarm-name bee-index))
                         (address (read-file-to-string acc-file)))
                    (display address))))))

         (shepherd-service
          (documentation (simple-format #f "Swarm bee node ~S in swarm ~S."
                                        bee-index swarm-name))
          (provision (list (bee-service-name swarm-name bee-index)))
          (requirement (append `(networking file-systems
                                            ,(clef-service-name swarm-name))
                               additional-service-requirements))
          (actions (list display-address-action))
          (modules +default-service-modules+)
          (start
           (let* ((data-dir (bee-data-directory swarm-name bee-index))
                  (account-file (bee-account-file swarm-name bee-index))
                  (bee-cfg (if singular?
                               bee-configuration
                               (bee-configuration-for-node-index
                                swarm bee-configuration bee-index)))
                  (config-file (plain-file
                                (simple-format #f "bee-config-~A.yaml" bee-index)
                                (with-output-to-string
                                  (lambda ()
                                    (serialize-configuration
                                     bee-cfg
                                     bee-configuration-fields))))))
             #~(lambda args
                 #$(swarm-service-gexp
                    service-config
                    #~(begin
                        (log.debug "Bee service is starting")
                        #$(bee-activation-gexp service-config config-file bee-index)
                        ;; Due to timing, we cannot add the node's eth address to
                        ;; the config file above, because it only gets generated
                        ;; at clef's service start time. Hence the extra round to
                        ;; pass it as an env variable at our own start time.
                        (let ((eth-address (when #$clef-signer-enable
                                             (read-file-to-string #$account-file)))
                              (cmd (list #$(file-append bee "/bin/bee")
                                         "--config" #$config-file
                                         "start")))
                          (log.dribble "About to fork+exec ~S" cmd)
                          (fork+exec-command
                           cmd
                           #:user #$bee-user
                           #:group #$swarm-group
                           #:log-file #$(bee-log-filename (default-log-directory swarm-name)
                                                          bee-index)
                           #:directory #$data-dir
                           #:resource-limits
                           `((nofile ,#$(+ db-open-files-limit 4096)
                                     ,#$(+ db-open-files-limit 4096)))
                           #:environment-variables
                           (list
                            (string-append "HOME=" #$data-dir)
                            ;; So that these are not visible with ps, or in the
                            ;; config file (i.e. world-readable under
                            ;; /gnu/store/), because they may contain keys when
                            ;; using a service like Infura.
                            (string-append "BEE_SWAP_ENDPOINT="    #$swap-endpoint)
                            (string-append "BEE_RESOLVER_OPTIONS=" #$resolver-options)
                            (string-append "BEE_CLEF_SIGNER_ETHEREUM_ADDRESS="
                                           (or eth-address ""))
                            "LC_ALL=en_US.UTF-8"))))))))
          (stop #~(make-kill-destructor))))))))

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
         (let ((commit "v0.9.0"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/ethersphere/bee-clef.git")
                   (commit commit)))
             (file-name (git-file-name "bee-clef" commit))
             (sha256
              (base32
               "1vg1mqzldfxc4agff0aw5a94453f22aj9a0grl4y0vs1bg5skcdd"))))))
    (file-append bee-clef-git relative-path)))

(define (rules.js)
  (upstream-bee-clef-file "/packaging/rules.js"))

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

            (define (ensure-clef-account account-file)
              (log.dribble "ENSURE-CLEF-ACCOUNT called for ~S" account-file)
              (unless (file-exists? account-file)
                (log.debug "Adding clef account for ~S" account-file)
                ;; TODO comment on why the lowercasing is done, or delete it.
                (let ((cmd (string-append
                            "echo -n $({ "
                            (build-clef-cmd "newaccount --lightkdf 2>/dev/null << EOF
$CLEF_PASSWORD
EOF
")
                            "} | tail -n -1 | cut -d'x' -f2 | tr '[:upper:]' '[:lower:]') >"
                            account-file)))
                  (invoke-clef-cmd cmd)
                  (chmod account-file #o440))
                (let ((eth-address (read-file-to-string account-file)))
                  (invoke-clef-cmd (build-clef-cmd
                                    "setpw 0x"eth-address" >/dev/null 2>&1 << EOF
$CLEF_PASSWORD
$CLEF_PASSWORD
$CLEF_PASSWORD
EOF
")))))

            (ensure-directories 0 "swarm" #o2775
                                #$+service-log-directory+
                                #$+service-data-directory+)

            (ensure-directories/rec clef-user-id clef-group-id #o750
                                    #$data-dir)

            (ensure-directories/rec clef-user-id clef-group-id #o700
                                    #$(clef-keystore-directory swarm-name))

            (ensure-password-file clef-pwd-file clef-user-id clef-group-id)

            (log.debug "Clef activation ensured the password and the keystore")

            ;; We need to ensure the bee data dirs already exist at this point,
            ;; so that we can write the eth addresses into them.  Note:
            ;;
            ;;   - this calls chown -R, and it shouldn't be executed when the
            ;;   bee node is already running (leads to probabilistic bug of file
            ;;   not found coming from chown).
            ;;
            ;;   - needs to be run as root, i.e. not inside the
            ;;   INVOKE-AS-CLEF-USER below.
            #$@(map (lambda (bee-index)
                      #~(ensure-directories/rec
                         bee-user-id bee-group-id #o2770
                         #$(bee-data-directory swarm-name bee-index)))
                    (iota node-count))

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
                   (invoke-clef-cmd (build-clef-cmd
                                     "attest $(sha256sum "
                                     #$(rules.js)
                                     " | cut -d' ' -f1 | tr -d '\n') "
                                     ">/dev/null 2>&1 << EOF
$CLEF_PASSWORD
EOF")))
                 ;; Ensure that each bee node has a clef account
                 #$@(map (lambda (bee-index)
                           #~(ensure-clef-account #$(bee-account-file swarm-name bee-index)))
                         (iota node-count))))))))))

(define (bee-activation-gexp service-config config-file bee-index)
  (match-record service-config <swarm-service-configuration>
      (swarm bee-configuration bee)
    (match-record bee-configuration <bee-configuration>
        (mainnet)
      (match-record swarm <swarm>
          ((name swarm-name))
        #~(let* ((data-dir      #$(bee-data-directory swarm-name bee-index))
                 (libp2p-key    (string-append data-dir "/keys/libp2p.key")))

            (ensure-directories/rec bee-user-id bee-group-id #o2770 data-dir)
            (ensure-password-file #$(bee-password-file swarm-name) bee-user-id bee-group-id)

            ;; When first started, call `bee init` for this bee instance.
            (unless (file-exists? libp2p-key)
              (log.debug "Invoking `bee init` for bee-index ~S in swarm ~S" #$bee-index #$swarm-name)
              (invoke-as-user bee-pw
                              (lambda ()
                                (invoke/quiet #$(file-append bee "/bin/bee")
                                              "--config" #$config-file
                                              "init")))))))))

(define (swarm-service-gexp service-config body-gexp)
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

              (log.dribble "A SWARM-SERVICE-GEXP is running for swarm ~S" swarm-name)

              (let ((dir #$(swarm-data-directory swarm-name)))
                ;; The data dir is visible to everyone.
                (ensure-directories 0 #$swarm-group #o2770 dir)
                (log.debug "Ensured directory ~S" dir))

              #$body-gexp))))))

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
               (uid (or (defined-value? clef-user-id) #false))
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
                (id (or (defined-value? swarm-group-id) #false))
                (system? #t))
               (user-account
                (name bee-user)
                (uid (or (defined-value? bee-user-id) #false))
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

(define* (swarm-service #:key
                        (node-count 1)
                        (resolver-options "")
                        (swap-endpoint "ws://localhost:8546")
                        (swap-initial-deposit *unspecified*)
                        (swarm swarm/mainnet)
                        (dependencies '())
                        (bee-supplementary-groups '())
                        (debug-api-enable #false)
                        (global-pinning-enable #false)
                        (db-open-files-limit +db-open-files-limit/default+))
  (service swarm-service-type
           (swarm-service-configuration
            (swarm                           swarm)
            (node-count                      node-count)
            (bee-supplementary-groups        bee-supplementary-groups)
            (additional-service-requirements dependencies)
            (bee-configuration
             (bee-configuration
              (resolver-options      resolver-options)
              (swap-endpoint         swap-endpoint)
              (swap-initial-deposit  swap-initial-deposit)
              (debug-api-enable      debug-api-enable)
              (data-dir              (bee-data-directory (swarm-name swarm) 0))
              (global-pinning-enable global-pinning-enable)
              (db-open-files-limit   db-open-files-limit))))))
