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

;;; TODO this is copied from openethereum.scm; there's quite a lot of overlap
;;;      waiting to be factored out.

(define-module (guix-crypto services nethermind)
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
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages tls)
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
  #:export (nethermind-service-configuration
            nethermind-service-configuration?
            nethermind-configuration
            nethermind-configuration?
            nethermind-service-type
            nethermind-service))

;;;
;;; Configuration
;;;
(define (serialize-field field-name value)
  (if (eq? field-name 'extra-arguments)
      (map (lambda (entry)
             (first (serialize-field (first entry) (second entry))))
           value)
      (list (string-append "--" (if (symbol? field-name)
                                    (symbol->string field-name)
                                    field-name)
                           "="
                           (if (string? value)
                               value
                               (object->string value))))))

(define (serialize-field/boolean field-name val)
  (list (string-append "--" (symbol->string field-name)
                       (if val "=true" "=false"))))

(define serialize-string  serialize-field)
(define serialize-list    serialize-field)
(define serialize-boolean serialize-field/boolean)
(define serialize-integer serialize-field)
(define serialize-non-negative-integer serialize-field)
(define serialize-service-name serialize-field)
;; (define (serialize-list field-name val)
;;   (map (cut serialize-field field-name <>) val))

(define-maybe string)
(define-maybe list)
(define-maybe boolean)
(define-maybe integer)
(define-maybe non-negative-integer)

;; https://docs.nethermind.io/nethermind/first-steps-with-nethermind/getting-started
;;
;; NOTE: the use of maybe-foo, even when there's a default value, makes
;; it possible to set it to %unset-value to use the default encoded in
;; the binary.
(define-configuration nethermind-configuration
  ;; For simplicity the field names here are the same as the
  ;; Nethermind config entry names.
  (config                maybe-string
   "Which blockchain to connect to. It should be the name of a config \
file under /share/nethermind-binary-1.2.3/configs/ (the .cfg extension is
optional in this case), or a full file path.")
  (datadir               maybe-string
   "Directory where the state is stored. Defaults to '/var/lib/nethermind/[service name]/.")
  (log                   maybe-string
   "Possible values: OFF|TRACE|DEBUG|INFO|WARN|ERROR")

  ;; Init (fully mirrored as of 1.17.3)
  (Init.AutoDump         maybe-string ; TODO type: (member '(None Receipts Parity Geth All))
   "")
  (Init.BaseDbPath       maybe-string ; TODO type: file path
   "")
  (Init.ChainSpecPath    maybe-string ; TODO type: file path
   "")
  (Init.DiagnosticMode   maybe-string
   "")
  (Init.DiscoveryEnabled maybe-boolean
   "")
  (Init.EnableUnsecuredDevWallet maybe-boolean
   "")
  (Init.GenesisHash      maybe-string
   "")
  (Init.HiveChainSpecPath maybe-string ; TODO type: file path
   "")
  (Init.IsMining         maybe-boolean
   "")
  (Init.KeepDevWalletInMemory maybe-boolean
   "")
  (Init.LogDirectory     maybe-string ; TODO type: file path
   "")
  (Init.LogFileName      maybe-string
   "")
  (Init.LogRules         maybe-string
   "Overrides for default logs in format LogPath:LogLevel;*")
  (Init.MemoryHint       maybe-non-negative-integer
   "")
  (Init.PeerManagerEnabled maybe-boolean
   "")
  (Init.ProcessingEnabled maybe-boolean
   "")
  (Init.ReceiptsMigration maybe-boolean
   "")
  (Init.RpcDbUrl          maybe-string ; TODO type: url
   "")
  (Init.StaticNodesPath   maybe-string ; TODO type: file path
   "")
  (Init.StoreReceipts     maybe-boolean
   "")
  (Init.WebSocketsEnabled maybe-boolean
   "")

  ;; JsonRpc
  (JsonRpc.Enabled       maybe-boolean
   "")
  (JsonRpc.Host          maybe-string
   "")
  (JsonRpc.Port          maybe-non-negative-integer
   "")
  (JsonRpc.EngineHost    maybe-string
   "")
  (JsonRpc.EnginePort    maybe-non-negative-integer
   "")
  (JsonRpc.UnsecureDevNoRpcAuthentication maybe-boolean
   "")
  (JsonRpc.WebSocketsPort maybe-non-negative-integer
   "")
  (JsonRpc.JwtSecretFile maybe-string
   "")

  (HealthChecks.Enabled  maybe-boolean
   "")
  (HealthChecks.UIEnabled maybe-boolean
   "")

  ;; network
  (Network.Bootnodes     maybe-string
   "")
  (Network.EnableUPnP    maybe-boolean
   "")
  (Network.OnlyStaticPeers maybe-boolean
   "")
  (Network.MaxActivePeers maybe-non-negative-integer
   "")
  (Network.P2PPort       maybe-non-negative-integer
   "")
  (Network.StaticPeers   maybe-string
   "")

  ;; pruning
  (Pruning.CacheMb       maybe-non-negative-integer
   "")
  (Pruning.Enabled       maybe-boolean
   "")
  (Pruning.FullPruningCompletionBehavior maybe-string ; TODO type: (member None ShutdownOnSuccess AlwaysShutdown)
   "")
  (Pruning.FullPruningMaxDegreeOfParallelism maybe-non-negative-integer
   "")
  (Pruning.FullPruningMemoryBudgetMb maybe-non-negative-integer
   "")
  (Pruning.FullPruningMinimumDelayHours maybe-non-negative-integer
   "")
  (Pruning.FullPruningThresholdMb maybe-non-negative-integer
   "")
  (Pruning.FullPruningTrigger maybe-string ; TODO type: (member Manual StateDbSize VolumeFreeSpace)
   "")
  (Pruning.Mode          maybe-string ; TODO type: (member None Memory Full Hybrid)
   "Possible values: 'None', 'Memory', 'Full', 'Hybrid'.")
  (Pruning.PersistenceInterval maybe-non-negative-integer
   "")

  ;; sync
  (Sync.DownloadBodiesInFastSync maybe-boolean
   "")
  (Sync.DownloadHeadersInFastSync maybe-boolean
   "")
  (Sync.DownloadReceiptsInFastSync maybe-boolean
   "")
  (Sync.FastBlocks       maybe-boolean
   "")
  (Sync.FastSync         maybe-boolean
   "")
  (Sync.FastSyncCatchUpHeightDelta maybe-non-negative-integer
   "")
  (Sync.NetworkingEnabled maybe-boolean
   "")
  (Sync.PivotHash        maybe-string ; TODO add more specific type?
   "")
  (Sync.PivotNumber      maybe-non-negative-integer
   "")
  (Sync.PivotTotalDifficulty maybe-string ; TODO what is the type here?
   "")
  (Sync.SnapSync         maybe-boolean
   "")
  (Sync.SnapSyncAccountRangePartitionCount maybe-non-negative-integer
   "")
  (Sync.StrictMode       maybe-boolean
   "")
  (Sync.SynchronizationEnabled maybe-boolean
   "")
  (Sync.UseGethLimitsInFastBlocks maybe-boolean
   "")
  (Sync.WitnessProtocolEnabled maybe-boolean
   "")

  (extra-arguments          maybe-list
   "A list of (name value) pairs that will be appended as-is to the end of the generated command line."))

(define-configuration/no-serialization nethermind-service-configuration
  ;;
  ;; Guix packages
  ;;
  (nethermind          (file-like nethermind-binary)
   "The Guix package to use.")
  ;;
  ;; Service's config
  ;;
  (service-name          service-name
   "The name of this Guix service; a symbol. It will be used as the \
PROVISION value of the Shepherd service, as a path component in the \
data and log directories, and in various defaults. You typically want \
to use here the same value you provided as the CONFIG (the name of \
the chain).")
  (user                  maybe-string
   "Unix user for the service process.")
  (user-id               maybe-non-negative-integer
   "Unix uid for @code{user}.")
  (group                 maybe-string
   "Unix group for @code{user} and the service processes.")
  (group-id              maybe-non-negative-integer
   "Unix gid for @code{group}.")
  (nethermind-configuration nethermind-configuration
   "Configuration for the Nethermind binary."))

(define (nethermind-configuration->cmd-arguments config)
  (fold (lambda (field result)
          (let ((name (configuration-field-name field))
                (value ((configuration-field-getter field) config)))
            (if (maybe-value-set? value)
                (append ((configuration-field-serializer field) name value)
                        result)
                result)))
        '()
        nethermind-configuration-fields))

(define (chain-name-from-config nm-config-field)
  ;; TODO handle the case when nm-config is a full path to a config file
  nm-config-field)

(define (apply-config-defaults cfg)
  (match-record cfg <nethermind-service-configuration>
      (user
       group
       service-name
       (nethermind-configuration nm-config))
    (match-record nm-config <nethermind-configuration>
        ((config nm-config-field)
         datadir
         Init.LogDirectory
         Init.LogFileName
         JsonRpc.JwtSecretFile)
      (let* ((chain-name (chain-name-from-config nm-config-field))
             (service-name (ensure-string
                            (maybe-value service-name
                                         (string-append "nm-" chain-name)))))
        (nethermind-service-configuration
         (inherit cfg)
         (user         (maybe-value user (string-append "nm-" chain-name)))
         (group        (maybe-value group "nethermind"))
         (service-name service-name)
         (nethermind-configuration
          (nethermind-configuration
           (inherit nm-config)
           (datadir     (ensure-string
                         (maybe-value datadir
                                      (string-append "/var/lib/nethermind/"
                                                     service-name))))
           (Init.LogDirectory (ensure-string
                               (maybe-value Init.LogDirectory
                                            "/var/log/nethermind")))
           (Init.LogFileName (string-append service-name ".log"))
           (JsonRpc.JwtSecretFile (maybe-value JsonRpc.JwtSecretFile
                                               (string-append datadir "/jwt-secret"))))))))))

;;;
;;;
;;;
(define (make-shepherd-service cfg)
  (set! cfg (apply-config-defaults cfg))
  (with-service-gexp-modules '()
    (match-record cfg <nethermind-service-configuration>
        (user group service-name nethermind nethermind-configuration)
      (match-record nethermind-configuration <nethermind-configuration>
          ((config nm-config-field)
           datadir
           Init.LogDirectory
           JsonRpc.JwtSecretFile)
        (list
         (shepherd-service
          (documentation (simple-format #f "An nethermind node connecting to chain '~A'"
                                        nm-config-field))
          (provision (list (string->symbol service-name)))
          (requirement '(networking file-systems))
          (modules +default-service-modules+)
          (start
           (let ((log-dir Init.LogDirectory))
             #~(lambda args
                 (with-log-directory #$log-dir
                   (let* ((path-list (list #$(file-append coreutils "/bin")
                                           #$(file-append which "/bin")))
                          (path-count (length path-list))
                          (path-string (apply string-append
                                              (take (append-map list path-list
                                                                (list-tabulate path-count
                                                                               (const ":")))
                                                    (1- (* path-count 2))))))
                     (setenv "PATH" path-string)

                     (ensure-directories 0 "nethermind" #o2771
                                         "/var/log/nethermind")

                     (log.debug "Nethermind service is starting up, log initialized")
                     (log.dribble "PATH is ~S" (getenv "PATH"))
                     (log.dribble "SHELL is ~S" (getenv "SHELL"))

                     (ensure-directories 0 "nethermind" #o2771
                                         "/var/lib/nethermind")

                     (log.dribble "/var/lib/nethermind initialized")

                     (ensure-directories/rec #$user #$group #o2751
                                             #$datadir)

                     (log.dribble "Nethermind data dir is initialized (~S)" #$datadir)

                     ;; Ensure we have a JWT secret. It should be a 64 digit hexadecimal number.
                     ;; openssl rand -hex 32 | tr -d "\n"
                     (let* ((password-file #$JsonRpc.JwtSecretFile)
                            (cmd (string-append
                                  #$(file-append openssl "/bin/openssl")
                                  " rand -hex 32 | "
                                  #$(file-append coreutils "/bin/tr")
                                  " -d \"\\n\" >\""
                                  password-file
                                  "\"")))
                       (if (file-exists? password-file)
                           (log.dribble "JWT secret already exists in ~S" password-file)
                           (begin
                             (log.debug "Generating JWT secret into ~S" password-file)
                             (unless (zero? (system cmd))
                               (error "Failed to generate JWT secret" password-file))))
                       (chmod password-file #o440)
                       (chown password-file
                              (or (ensure-uid #$user)  -1)
                              (or (ensure-gid #$group) -1)))

                     (let ((cmd (append
                                 '#$(cons*
                                     (file-append nethermind "/bin/Nethermind.Runner")
                                     (nethermind-configuration->cmd-arguments
                                      nethermind-configuration))
                                 ;; Allow `herd start nethermind-srv --more-args-for-nethermind
                                 args)))

                       (log.debug "Will exec ~S" cmd)

                       (let* ((forkexec
                               (make-forkexec-constructor
                                cmd
                                #:user #$user
                                #:group #$group
                                ;; TODO FIXME i can't seem to configure nethermind's log,
                                ;; so let's just keep this as a backup for now.
                                ;; remember: it's not rotated! so, resolve this soon...
                                #:log-file #$(string-append log-dir "/" service-name ".stdout.log")
                                #:resource-limits `((nofile 8192 8192))
                                #:environment-variables
                                (append
                                 (list (string-append "HOME=" #$datadir)
                                       (string-append "PATH=" path-string))
                                 +root-environment+)))
                              (pid (forkexec)))
                         ;; TODO We should wait for some sign here that tells
                         ;; us that the daemon has finished starting up,
                         ;; otherwise any (requirement ...) specification on
                         ;; other services is useless. But the RPC socket
                         ;; comes online after minutes, not seconds.
                         pid)))))))
          (stop
           #~(make-kill-destructor #:grace-period 120))))))))

(define (make-unix-user-accounts cfg)
  (set! cfg (apply-config-defaults cfg))
  (match-record
      cfg <nethermind-service-configuration>
    (user user-id group group-id nethermind-configuration)
    (match-record
          nethermind-configuration <nethermind-configuration>
      ((config nm-config-field) datadir)
      ;; NOTE it's safe to forward the #false default value of uid/gid to
      ;; USER-ACCOUNT.
      (let ((chain-name (chain-name-from-config nm-config-field)))
        (append
         (if (equal? group "nethermind")
             '()
             (list
              (user-group
               (name "nethermind")
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
           (supplementary-groups (delete group '("nethermind")))
           (system? #t)
           (comment (string-append "Nethermind service for chain '" chain-name "'"))
           (home-directory datadir)
           (shell (file-append shadow "/sbin/nologin")))))))))

(define (make-nethermind-log-rotations service-config)
  (set! service-config (apply-config-defaults service-config))
  (match-record service-config <nethermind-service-configuration>
      (service-name nethermind-configuration)
    (match-record nethermind-configuration <nethermind-configuration>
        ((Init.LogDirectory log-dir)
         Init.LogFileName)
      (list
       (log-rotation
        (files (list
                (string-append log-dir "/service.log")
                (string-append log-dir "/" Init.LogFileName)))
        (frequency 'weekly)
        (options '("rotate 8")))))))

;;
;; Interfacing with Guix
;;
(define nethermind-service-type
  (service-type
   (name 'nethermind)
   (extensions
    ;; We can't use the ACTIVATION-SERVICE-TYPE here, because it executes
    ;; us too early, before e.g. the filesystems are mounted in case of `guix
    ;; system --share=... vm ...`
    (list (service-extension shepherd-root-service-type
                             make-shepherd-service)
          (service-extension account-service-type
                             make-unix-user-accounts)
          (service-extension rottlog-service-type
                             make-nethermind-log-rotations)))
   (description "Runs a Nethermind instance as a Shepherd service.")))

(define* (nethermind-service #:key
                             (user                %unset-value)
                             (group               %unset-value)
                             (config              "mainnet")
                             (service-name        'mainnet)
                             (datadir             %unset-value)
                             (HealthChecks.Enabled   #false)
                             (HealthChecks.UIEnabled #false)
                             (JsonRpc.Enabled     %unset-value)
                             (JsonRpc.Host        %unset-value)
                             (JsonRpc.Port        %unset-value)
                             (JsonRpc.EngineHost  %unset-value)
                             (JsonRpc.EnginePort  %unset-value)
                             (JsonRpc.UnsecureDevNoRpcAuthentication %unset-value)
                             (JsonRpc.WebSocketsPort %unset-value)
                             (JsonRpc.JwtSecretFile %unset-value)
                             (Sync.FastSync       #true)
                             (Sync.UseGethLimitsInFastBlocks #true))
  (service
   nethermind-service-type
   (nethermind-service-configuration
    (service-name            service-name)
    (user                    user)
    (group                   group)
    (nethermind-configuration
     (nethermind-configuration
      (config                config)
      (datadir               datadir)
      (JsonRpc.Enabled       JsonRpc.Enabled)
      (JsonRpc.Host          JsonRpc.Host)
      (JsonRpc.Port          JsonRpc.Port)
      (JsonRpc.EngineHost    JsonRpc.EngineHost)
      (JsonRpc.EnginePort    JsonRpc.EnginePort)
      (JsonRpc.UnsecureDevNoRpcAuthentication JsonRpc.UnsecureDevNoRpcAuthentication)
      (JsonRpc.WebSocketsPort JsonRpc.WebSocketsPort)
      (JsonRpc.JwtSecretFile JsonRpc.JwtSecretFile)
      (HealthChecks.Enabled  HealthChecks.Enabled)
      (HealthChecks.UIEnabled HealthChecks.UIEnabled)
      (Sync.FastSync         Sync.FastSync)
      (Sync.UseGethLimitsInFastBlocks Sync.UseGethLimitsInFastBlocks))))))
