;;; package --- Emacs init file

;;; Commentary:

;; Setup package manager and build configuration file.

;;; Code:

;; Increases garbage collection during startup
(defvar startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () "." (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

; --------------------------------------

(defvar emacs-cache (concat (getenv "XDG_CACHE_HOME") "/emacs")
  "Directory where Emacs cache data is stored.")
(defvar emacs-d (concat (getenv "HOME") "/.emacs.d")
  "Additional per-user Emacs-specific files.")

; --------------------------------------

;; Add the melpa repository to the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Activate autoloads
(package-initialize)

;; Install the 'use-package' dependency
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; --------------------------------------

;; Tangle configuration file
(require 'org)
(when (file-readable-p (concat emacs-d "/config.org"))
  (org-babel-load-file (concat emacs-d "/config.org")))

;;; init.el ends here
