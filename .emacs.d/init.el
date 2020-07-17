;;; init.el --- Emacs init file

;;; Commentary:

;; Setup package manager and build configuration file.

;;; Code:

;; Increases garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 8000000)))

; --------------------------------------

(defvar emacs-cache (concat (getenv "XDG_CACHE_HOME") "/emacs")
  "Directory where Emacs cache data is stored.")
(defvar emacs-d (concat (getenv "HOME") "/.emacs.d")
  "Directory where Emacs config files are stored.")

(defvar dot/leader-key "SPC"
  "Leader prefix key.")
(defvar dot/leader-alt-key "M-SPC"
  "Alternative leader prefix key, used for Insert and Emacs states.")
(defvar dot/localleader-key "SPC m"
  "Local leader prefix key, for 'major-mode' specific commands.")
(defvar dot/localleader-alt-key "M-SPC m"
  "Alternative local leader prefix key, used for Insert and Emacs states.")

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
