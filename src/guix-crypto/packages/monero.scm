;;; Copyright © 2023 Attila Lendvai <attila@lendvai.name>
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

(define-module (guix-crypto packages monero)
  #:use-module (guix-crypto utils)
  #:use-module (guix-crypto package-utils)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71))

(define-public feather-binary
  (let ((version hashes (read-hashes-file "feather-binary")))
    (package
      (name "feather-binary")
      (version version)
      (source
       (let* ((uri file-name
                   (feather-release-uri "ignored" version)))
         (origin
           (method url-fetch)
           (uri uri)
           (file-name file-name)
           (sha256 (base32 (or (assoc-ref hashes (%current-system))
                               (unsupported-arch name (%current-system))))))))
      ;;(outputs '("out" "clef" "evm"))
      (build-system binary-build-system)
      (arguments
       (let ((name-and-version (string-append "feather-" version)))
         (list
          #:imported-modules (source-module-closure
                              `((guix-crypto utils)
                                ,@%binary-build-system-modules)
                              #:select? default-module-filter)
          #:modules '((guix build utils)
                      (guix-crypto utils)
                      (nonguix build binary-build-system))
          #:strip-binaries? #f          ; The less we modify, the better.
          ;; TODO fontconfig-minimal is a guix anomaly: https://issues.guix.gnu.org/61292
          #:patchelf-plan `'((,name-and-version
                              ("gcc"
                               "glibc"
                               "fontconfig-minimal"
                               "libxkbcommon"
                               "libxcb"
                               "xcb-util-cursor"
                               "xcb-util-wm"
                               "xcb-util-image"
                               "xcb-util-keysyms"
                               "xcb-util-renderutil"
                               )))
          #:install-plan `'((,name-and-version
                             ,(string-append "bin/" name-and-version)))
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'install 'wrap-executable
                (lambda* (#:key inputs #:allow-other-keys)
                  (wrap-program (string-append #$output "/bin/" #$name-and-version)
                    ;; libssl.so is an optional dependency opened using dlopen
                    `("LD_LIBRARY_PATH" suffix
                      (,(string-append (assoc-ref inputs "openssl") "/lib")))
                    ;; TODO qt.qpa.plugin: Could not find the Qt platform plugin "wayland" in ""
                    ;; qt.qpa.theme.dbus: Session DBus not running.
                    ;; qt.qpa.theme.dbus: Application will not react to setting changes.
                    ;; Check your DBus installation.
                    `("QT_PLUGIN_PATH" ":" prefix
                      ,(map (lambda (label)
                              (string-append (assoc-ref inputs label)
                                             "/lib/qt5/plugins"))
                            '("qtbase" "qtwayland")))
                    `("QT_QPA_PLATFORM_PLUGIN_PATH" ":" =
                      ,(map (lambda (label)
                              (string-append (assoc-ref inputs label)
                                             "/lib/qt5/plugins/platforms"))
                            '("qtbase" "qtwayland"))))))
              (add-after 'wrap-executable 'symlink-binaries
                (lambda* (#:key #:allow-other-keys)
                  (let* ((source #$name-and-version)
                         (target "feather"))
                    (with-directory-excursion (string-append #$output "/bin/")
                      (format #t "~A -> ~A~%" source target)
                      (symlink source target)))))
              ;; TODO qt.qpa.plugin: Could not find the Qt platform plugin "offscreen" in "/gnu/store/5m7al14hrq0cmzgy8p1xqhnagdhfqv1x-qtbase-5.15.5/lib/qt5/plugins/platforms:/gnu/store/dmgv7isvff0g4pxpi6z0ry44hch0af9p-qtwayland-5.15.5/lib/qt5/plugins/platforms"
              ;; This application failed to start because no Qt platform plugin could be initialized. Reinstalling the application may fix this problem.
              ;; Available platform plugins are: xcb, xcb.
              ;; (add-after 'symlink-binaries 'check
              ;;   (lambda* (#:key (tests? #t) #:allow-other-keys)
              ;;     (when tests?
              ;;       (setenv "HOME" (getcwd))
              ;;       (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;;       (setenv "QT_QPA_PLATFORM" "offscreen")
              ;;       (invoke (string-append #$output "/bin/feather")
              ;;               "--version"))))
              ))))
      (native-inputs (list
                      gnupg
                      patchelf
                      unzip))
      (inputs (list fontconfig
                    (list gcc "lib")
                    glibc
                    libxkbcommon
                    libxcb
                    openssl
                    qtbase-5
                    qtwayland-5
                    xcb-util-cursor
                    xcb-util-wm
                    xcb-util-image
                    xcb-util-keysyms
                    xcb-util-renderutil))
      (supported-systems (map first hashes))
      (home-page "https://featherwallet.org/")
      (synopsis "A free, open-source Monero wallet")
      (description
       "Feather is a free, open-source Monero wallet for Linux, Tails, Windows and macOS.")
      (license license:bsd-3)
      (properties
       '((release-monitoring-url . "https://github.com/feather-wallet/feather/tags"))))))
