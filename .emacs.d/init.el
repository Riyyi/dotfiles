;;; init.el --- Emacs init file

;;; Commentary:

;; Setup package manager and build configuration file.

;;; Code:

;; Increases garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 8000000)))

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

;; Base directory
(setq user-emacs-directory (file-name-directory load-file-name))

;; Tangle configuration file
(require 'org)
(when (file-readable-p (concat user-emacs-directory "config.org"))
  (org-babel-load-file (concat user-emacs-directory "config.org")))

;;; init.el ends here
