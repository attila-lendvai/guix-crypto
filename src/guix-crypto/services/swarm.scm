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

(define-module (guix-crypto services swarm)
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
  #:export (swarm-configuration
            swarm-configuration?
            swarm-service-type
            swarm-service))


;;;
;;; Configuration
;;;
(define (undefined-value? x)
  (or (eq? x 'undefined)
      (eq? x 'disabled)))

(define (defined-value? x)
  (not (undefined-value? x)))

(define (swarm-name? val)
  (or (string? val)
      (member val '(mainnet testnet))))

(define (verbosity-value? val)
  (or (member val '(silent error warn info debug trace))
      (and (integer? val)
           (<= 0 val 5))))

(define (non-negative-integer? val)
  (and (exact-integer? val)
       (not (negative? val))))

(define-maybe non-negative-integer (no-serialization))
(define-maybe string               (no-serialization))
(define-maybe integer              (no-serialization))
(define-maybe boolean              (no-serialization))

;; The field names here mirror the bee config entry names.
(define-configuration swarm-configuration
  ;; Packages
  (bee                   (file-like bee-binary)
                         "The Bee Guix package to use.")
  (geth                  (file-like geth-binary)
                         "The go-ethereum (geth) package to use (for the \
@code{clef} binary).")
  (additional-service-requirements
   (list '())
   "Guix service names that are appended to the REQUIREMENT field of each \
Bee node's Shepherd service instance; i.e. here you can specify extra \
dependencies for the start order of the services, e.g. if you are running \
a local Gnosis chain node instance, then you can add its name here.")
  ;; Users and groups
  (bee-user              (maybe-string 'disabled)
                         "Unix user for the Bee processes in this swarm.")
  (bee-user-id           (maybe-non-negative-integer 'disabled)
                         "Unix uid for @code{bee-user}.")
  (clef-user             (maybe-string 'disabled)
                         "Unix user for the clef process in this swarm.")
  (clef-user-id          (maybe-non-negative-integer 'disabled)
                         "Unix uid for @code{clef-user}.")
  (swarm-group           (maybe-string 'disabled)
                         "Unix group for the users in this swarm.")
  (swarm-group-id        (maybe-non-negative-integer 'disabled)
                         "Unix gid for @code{swarm-group}.")
  ;; Swarm config
  ;; TODO validate it to be compatible with paths
  (swarm                 (swarm-name 'mainnet)
                         "Either one of the symbols @code{'testnet} or \
@code{'mainnet}, or a string when specifying the details of a custom swarm.")
  (clef-signer-enable    (boolean #true)
                         "Whether to connect to a clef instance, or the \
Bee node should be doing its own Ethereum key management.")
  (clef-chain-id         (maybe-non-negative-integer 'disabled)
                         "Blockchain ID for clef.")
  (node-count            (non-negative-integer 1)
                         "How many Bee nodes should be started.")
  ;; defaults to: 1600 for mainnet, 1900 for testnet
  (p2p-port-base         (maybe-non-negative-integer 'disabled)
                         "Base number for the p2p ports of the Bee nodes. \
Defaults to 1600 for @code{mainnet} and 1900 for @code{testnet}, otherwise \
it must be specified.")
  (api-port-base         (maybe-non-negative-integer 'disabled)
                         "Base number for the api ports of the Bee nodes. \
Defaults to 1700 for @code{mainnet} and 2000 for @code{testnet}, otherwise \
it must be specified.")
  (debug-api-port-base   (maybe-non-negative-integer 'disabled)
                         "Base number for the debug api ports of the Bee \
nodes. Defaults to 1800 for @code{mainnet} and 2100 for @code{testnet}, \
otherwise it must be specified.")
  (debug-api-enable      (boolean #false) "")
  ;; Bee node config
  (db-open-files-limit   (non-negative-integer 500) "") ; 2000+ is suggested, but what about the ulimit?
  (global-pinning-enable (boolean #false) "")
  (verbosity             (verbosity-value 'info)
                         "Either an integer between 0 and 5, or one of the \
symbols: @code{'silent}, @code{'error}, @code{'warn}, @code{'info}, \
@code{'debug}, @code{'trace}.")
  (full-node             (boolean #true) "")
  (swap-endpoint         (string) "A blockchain node endpoint to connect to.")
  (network-id            (maybe-non-negative-integer 'disabled) "Defaults to @code{1} for \
@code{mainnet}, and @code{10} for @code{testnet}; otherwise it must be \
specified.")
  (mainnet               (maybe-boolean 'disabled) "")
  (extra-bee-config      (maybe-string 'disabled) "A string that will be appended as-is \
to the end of the generated @code{bee.yml} files.")
  (no-serialization))

(define (apply-config-defaults config)
  (match-record config <swarm-configuration>
    (swarm mainnet network-id clef-chain-id
           bee-user clef-user swarm-group
           p2p-port-base api-port-base debug-api-port-base)

    (define* (default-port value mainnet-port testnet-port)
      (or (defined-value? value)
          (match swarm
            ('mainnet mainnet-port)
            ('testnet testnet-port))))

    (define (derived-field-values)
      (let ((rtd (record-type-descriptor config)))
        (map (lambda (field)
               ((record-accessor rtd field) config))
             +swarm-derived-configuration-fields+)))

    (define (assert-swarm-details-undefined)
      (let ((vals (derived-field-values)))
        (unless (every undefined-value? vals)
          (raise-missing-config-error swarm))))

    (let ((any-port-undefined? (any undefined-value?
                                    (list p2p-port-base
                                          api-port-base
                                          debug-api-port-base))))
      (when (and any-port-undefined?
                 (not (member swarm '(testnet mainnet))))
        (raise-missing-config-error swarm))

      ;; Return with a copy in which everything is fully specified.
      (let-values
          (((swarm-name mainnet network-id clef-chain-id)
            (match swarm
              ('mainnet
               (assert-swarm-details-undefined)
               (values "mainnet" #true  1  100))

              ('testnet
               (assert-swarm-details-undefined)
               (values "testnet" #false 10 5))

              ((? string? name)
               (let ((vals (derived-field-values)))
                 (when (any (negate undefined-value?) vals)
                   (raise-extra-config-error swarm)))
               (values name mainnet network-id clef-chain-id)))))

        (swarm-configuration
         (inherit config)
         (bee-user       (or (defined-value? bee-user)
                             (string-append "bee-"   swarm-name)))
         (clef-user      (or (defined-value? clef-user)
                             (string-append "clef-"  swarm-name)))
         (swarm-group    (or (defined-value? swarm-group)
                             (string-append "swarm-" swarm-name)))
         (p2p-port-base (default-port p2p-port-base 1600 1900))
         (api-port-base (default-port api-port-base 1700 2000))
         (debug-api-port-base (default-port debug-api-port-base 1800 2100))
         ;; Set some defaults for well-known swarms (i.e. 'mainnet or 'testnet).
         (swarm         swarm-name)
         (mainnet       mainnet)
         (network-id    network-id)
         (clef-chain-id clef-chain-id))))))

(define +swarm-derived-configuration-fields+
  '(network-id mainnet clef-chain-id))

(define (raise-missing-config-error swarm)
  (raise (formatted-message (G_ "When you specify '~S' as a custom swarm \
(i.e. using a string as the swarm's name), then you must also specify the \
following configuration values: ~{~A, ~}.")
                            swarm (append
                                   +swarm-derived-configuration-fields+
                                   '(p2p-port-base
                                     api-port-base
                                     debug-api-port-base)))))

(define (raise-extra-config-error swarm)
  (raise (formatted-message (G_ "When you specify '~S' as swarm (i.e. using \
the symbols 'testnet or 'mainnet as the swarm's name), then you must not \
specify the following configuration values: ~{~A, ~}.")
                            swarm +swarm-derived-configuration-fields+)))

(define (scheme-boolean->string value)
  (if value "true" "false"))

;; TODO maybe use serialize-configuration
(define (serialize-bee-config config bee-index)
  (match-record config <swarm-configuration>
    (swarm mainnet network-id full-node api-port-base p2p-port-base
               debug-api-port-base debug-api-enable db-open-files-limit
               global-pinning-enable verbosity)
    (string-append
     "mainnet: "          (scheme-boolean->string mainnet) "\n"
     "network-id: \""     (number->string network-id) "\"\n"
     "full-node: "        (scheme-boolean->string full-node) "\n"
     "data-dir: "         (bee-data-directory swarm bee-index) "\n"
     "api-addr: :"        (number->string (+ api-port-base bee-index)) "\n"
     "p2p-addr: :"        (number->string (+ p2p-port-base bee-index)) "\n"
     "debug-api-addr: :"  (number->string (+ debug-api-port-base bee-index)) "\n"
     "debug-api-enable: " (scheme-boolean->string debug-api-enable) "\n"
     "db-open-files-limit: \"" (number->string db-open-files-limit) "\"\n"
     "global-pinning-enable: \"" (scheme-boolean->string global-pinning-enable) "\"\n"
     "password-file: \""  (bee-password-file swarm) "\"\n"
     ;; NOTE we will pass clef-signer-enable as a CLI arg, so that the `bee
     ;; init` phase doesn't want to connect to clef.
     "clef-signer-endpoint: " (clef-ipc-file swarm) "\n"
     "verbosity: "        (object->string verbosity) "\n"
     )))


;;;
;;; Service implementation
;;;
(define-public (default-log-directory swarm-name)
  (string-append "/var/log/swarm/" swarm-name))

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

(define (make-shepherd-service/clef config)
  (with-service-gexp-modules '()
    (match-record config <swarm-configuration>
        (swarm mainnet network-id clef-chain-id geth clef-user swarm-group)
      (shepherd-service
       (documentation (simple-format #f "Swarm clef instance in swarm ~S."
                                     swarm))
       (provision (list (clef-service-name swarm)))
       (requirement '(networking file-systems))
       (modules +default-service-modules+)
       (start
        (let* ((data-dir     (clef-data-directory     swarm))
               (keystore-dir (clef-keystore-directory swarm))
               (4byte.json   (upstream-bee-clef-file "/packaging/4byte.json"))
               (rules.js     (upstream-bee-clef-file "/packaging/rules.js"))
               (start-script (local-file "swarm-clef-start-script"))
               (cmd #~(list #$(file-append bash-minimal "/bin/bash")
                            #$start-script
                            (string-append #$geth:clef "/bin/clef")
                            #$data-dir #$keystore-dir
                            #$(number->string clef-chain-id)
                            #$rules.js #$4byte.json)))
          ;; TODO it would be nice to get rid of the start shell script, but
          ;; i don't want to get into pipes and stuff in scheme when
          ;; upstream has already put together a shell script.
          #~(lambda args
              #$(swarm-service-gexp
                 config
                 #~(begin
                     (log.debug "Clef service is starting up")

                     #$(clef-activation-gexp config)

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
                                             (call-with-input-file
                                                 #$(clef-password-file swarm)
                                               (lambda (port)
                                                 (get-string-all port))))
                              "LC_ALL=en_US.UTF-8")))

                     ;; We need to do this here, because we must not return
                     ;; from start until clef is properly up and running,
                     ;; otherwise the (requirement `(clef-service-name)) is
                     ;; useless on the bee service.
                     (let ((pid (apply forkexec args)))
                       (ensure-ipc-file-permissions pid #$(clef-ipc-file swarm))
                       pid))))))
       (stop #~(make-kill-destructor))))))

(define (make-shepherd-service/bee bee-index config)
  (with-service-gexp-modules '()
   (match-record config <swarm-configuration>
       (swarm mainnet network-id bee bee-user swarm-group node-count
              swap-endpoint full-node clef-signer-enable
              additional-service-requirements)

     (define display-address-action
       (shepherd-action
        (name 'display-address)
        (documentation "Print the Bee node's Ethereum address.")
        (procedure
         #~(lambda _
             (let* ((acc-file #$(bee-account-file swarm bee-index))
                    (address (read-file-to-string acc-file)))
               (display address))))))

     (shepherd-service
      (documentation (simple-format #f "Swarm bee node ~S in swarm ~S."
                                    bee-index swarm))
      (provision (list (bee-service-name swarm bee-index)))
      (requirement (append `(networking file-systems
                                        ,(clef-service-name swarm))
                            additional-service-requirements))
      (actions (list display-address-action))
      (modules +default-service-modules+)
      (start
       (let* ((data-dir (bee-data-directory swarm bee-index))
              (account-file (bee-account-file swarm bee-index))
              (config-file (plain-file "bee-config.yaml"
                                       (serialize-bee-config config bee-index))))
         #~(lambda args
             #$(swarm-service-gexp
                config
                #~(begin
                    (log.debug "bee service start")
                    #$(bee-activation-gexp config config-file bee-index)
                    ;; Due to timing, we cannot add the node's eth address to
                    ;; the config file above, because it only gets generated
                    ;; at clef's service start time. Hence the extra round to
                    ;; pass it as a CLI argument at our own start time.
                    (let ((eth-address (when #$clef-signer-enable
                                         (call-with-input-file #$account-file
                                           (lambda (port)
                                             (get-string-all port)))))
                          (cmd (list #$(file-append bee "/bin/bee")
                                     "--config" #$config-file
                                     "start")))
                      (fork+exec-command
                       (if #$clef-signer-enable
                           (append cmd
                               (list "--clef-signer-enable"))
                           cmd)
                       #:user #$bee-user
                       #:group #$swarm-group
                       #:log-file (string-append
                                   (*log-directory*)
                                   #$(simple-format #f "/bee-~A.log" bee-index))
                       #:directory #$data-dir
                       #:environment-variables
                       (list
                        (string-append "HOME=" #$data-dir)
                        ;; So that it's not visible with ps (it may
                        ;; contain keys when using a remote service).
                        (string-append "BEE_SWAP_ENDPOINT="
                                       #$swap-endpoint)
                        (string-append "BEE_CLEF_SIGNER_ETHEREUM_ADDRESS="
                                       eth-address)
                        "LC_ALL=en_US.UTF-8"))))))))
      (stop #~(make-kill-destructor))))))

(define (make-swarm-shepherd-services config)
  (set! config (apply-config-defaults config))
  (match-record config <swarm-configuration>
    (node-count clef-signer-enable)
    (append
     (if clef-signer-enable
         (list (make-shepherd-service/clef config))
         '())
     (map (lambda (bee-index)
            (make-shepherd-service/bee bee-index config))
          (iota node-count)))))

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

(define (clef-activation-gexp config)
  (match-record config <swarm-configuration>
    (swarm geth node-count clef-user)
    (let ((4byte.json    (upstream-bee-clef-file "/packaging/4byte.json"))
          (rules.js      (upstream-bee-clef-file "/packaging/rules.js"))
          (data-dir      (clef-data-directory swarm))
          (keystore-dir  (clef-keystore-directory swarm)))
      #~(let* ((clef-pwd-file #$(clef-password-file swarm))
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

          (define (invoke-as-clef-user thunk)
            (log.dribble "INVOKE-AS-CLEF-USER called")
            (invoke-as-user clef-pw thunk))

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
                (invoke-clef-cmd cmd))
              (let ((eth-address (read-file-to-string account-file)))
                (invoke-clef-cmd (build-clef-cmd
                                  "setpw 0x"eth-address" >/dev/null 2>&1 << EOF
$CLEF_PASSWORD
$CLEF_PASSWORD
$CLEF_PASSWORD
EOF
")))))

          (mkdir+chown-r #$data-dir #o750 clef-user-id clef-group-id)
          (mkdir+chown-r #$(clef-keystore-directory swarm)
                         #o700 clef-user-id clef-group-id)

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
                    #~(mkdir* #$(bee-data-directory swarm bee-index)))
                  (iota node-count))

          (invoke-as-clef-user
           (lambda ()
             (log.debug "Initializing clef for swarm ~S" #$swarm)
             ;; Sending the password in the command line would expose it.
             (setenv "CLEF_PASSWORD" (read-file-to-string clef-pwd-file))
             (setenv "PATH" #$(file-append coreutils "/bin"))
             (let ((master-seed (string-append #$data-dir "/masterseed.json")))
               ;; Ensure that clef is initialized
               (unless (file-exists? master-seed)
                 (log.debug "Generating clef master seed at ~S for swarm ~S" master-seed #$swarm)
                 (invoke-clef-cmd (build-clef-cmd "init >/dev/null 2>&1 << EOF
$CLEF_PASSWORD
$CLEF_PASSWORD
EOF"))
                 (invoke-clef-cmd (build-clef-cmd
                                   "attest $(sha256sum "
                                   #$rules.js
                                   " | cut -d' ' -f1 | tr -d '\n') "
                                   ">/dev/null 2>&1 << EOF
$CLEF_PASSWORD
EOF")))
               ;; Ensure that each bee node has a clef account
               #$@(map (lambda (bee-index)
                         #~(ensure-clef-account #$(bee-account-file swarm bee-index)))
                       (iota node-count)))))))))

(define (bee-activation-gexp config config-file bee-index)
  (match-record config <swarm-configuration>
    (swarm mainnet network-id bee node-count)
    #~(let* ((data-dir      #$(bee-data-directory swarm bee-index))
             (libp2p-key    (string-append data-dir "/keys/libp2p.key")))

        (mkdir+chown-r data-dir)

        (ensure-password-file #$(bee-password-file swarm) bee-user-id bee-group-id)

        ;; When first started, call `bee init` for this bee instance.
        (unless (file-exists? libp2p-key)
          (log.debug "Invoking `bee init` for bee-index ~S in swarm ~S" #$bee-index #$swarm)
          (invoke-as-bee-user
           (lambda ()
             (invoke/quiet #$(file-append bee "/bin/bee")
                           "--config" #$config-file
                           "init")))))))

(define (swarm-service-gexp config body-gexp)
  "Returns a GEXP that is called before the start of any of the services.  It
means that this GEXP should only do operations that are safe to be called any
number of times, in any random moment."
  (match-record config <swarm-configuration>
      (swarm mainnet network-id node-count bee-user swarm-group)
    ;; TODO use the -foo- naming convention for the implicit bindings below?
    #~(let* ((swarm         #$swarm)
             (path          #$(file-append coreutils "/bin"))
             (bash          #$(file-append bash-minimal "/bin/bash"))
             (tr            #$(file-append coreutils "/bin/tr"))
             (head          #$(file-append coreutils "/bin/head"))
             (chown-bin     #$(file-append coreutils "/bin/chown"))
             (bee-pw        (getpwnam #$bee-user))
             (bee-user-id   (passwd:uid bee-pw))
             (bee-group-id  (passwd:gid bee-pw)))

        (with-log-directory #$(default-log-directory swarm)
          ;; so that we can already invoke stuff before the fork+exec
          (setenv "PATH" path)

          ;; TODO move as much of this as possible into
          ;; swarm-utils.scm. The headache is that those functions
          ;; cannot refer to store paths knowing only the package
          ;; objects, or i don't know how.

          (define* (mkdir* dir #:optional (mode #o2770) ; group sticky
                           (user-id bee-user-id) (group-id bee-group-id))
            (mkdir-p dir)
            (chmod dir mode)
            (chown dir user-id group-id))

          (define* (mkdir+chown-r dir #:optional (mode #o2770) ; group sticky
                                  (user-id bee-user-id) (group-id bee-group-id))
            (mkdir*  dir mode user-id group-id)
            (chown-r user-id group-id dir))

          (define (invoke-as-bee-user thunk)
            (log.dribble "INVOKE-AS-BEE-USER called")
            (invoke-as-user bee-pw thunk))

          ;;
          ;; Ensure basic directory structure
          ;;
          (mkdir* (*log-directory*))

          (let ((path (service-log-filename)))
            ;; ensure as root tghat the service.log file exists, and
            ;; with the right perms.
            (close-port (open-file path "a"))
            (chmod path #o660))

          ;;
          ;; Service logging is set up
          ;;

          ;;(log.debug* "guile version: ~A" (version))
          (log.dribble "A SWARM-SERVICE-GEXP is running for swarm ~S" swarm)

          (let ((path #$(swarm-data-directory swarm)))
            (mkdir-p path)
            (chmod   path #o2775)
            (chown   path 0 (group:gid (getgrnam #$swarm-group))))

          (log.debug "Ensured directory ~S" #$(swarm-data-directory swarm))

          #$body-gexp))))

(define (make-swarm-user-accounts config)
  (set! config (apply-config-defaults config))
  (match-record config <swarm-configuration>
    (swarm clef-signer-enable bee-user bee-user-id clef-user clef-user-id
                        swarm-group swarm-group-id)
    ;; NOTE it's safe to forward the #false default value of uid/gid to
    ;; USER-ACCOUNT.
    (append
     (if clef-signer-enable
         (list
          (user-account
           (name clef-user)
           (uid (or (defined? clef-user-id) #false))
           (group swarm-group)
           (system? #t)
           (comment (string-append "Service user for the clef instance of swarm "
                                   swarm))
           (home-directory (swarm-data-directory swarm))
           (shell (file-append shadow "/sbin/nologin"))))
         '())
     (list (user-group
            (name swarm-group)
            (id (or (defined? swarm-group-id) #false))
            (system? #t))
           (user-account
            (name bee-user)
            (uid (or (defined? bee-user-id) #false))
            (group swarm-group)
            (system? #t)
            (comment (string-append "Swarm Bee service user for swarm "
                                    swarm))
            (home-directory (swarm-data-directory swarm))
            (shell (file-append shadow "/sbin/nologin")))))))


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
                             make-swarm-user-accounts)))))

(define* (swarm-service #:key
                        (node-count 1)
                        (swap-endpoint "ws://localhost:8546")
                        (swarm 'mainnet)
                        (dependencies '()))
  (service swarm-service-type
           (swarm-configuration
            (node-count node-count)
            (swap-endpoint swap-endpoint)
            (swarm swarm)
            (additional-service-requirements dependencies))))
