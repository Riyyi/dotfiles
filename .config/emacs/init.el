;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Setup package archives and build configuration file.

;;; Code:

(require 'package)

;; Add the MELPA repository to the package manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Install the `use-package' dependency
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is complete
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; -------------------------------------

;; Tangle and load configuration file
(require 'org)
(when (file-readable-p (concat user-emacs-directory "config.org"))
  (org-babel-load-file (concat user-emacs-directory "config.org")))

;;; init.el ends here
