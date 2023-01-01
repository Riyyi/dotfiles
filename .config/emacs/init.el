;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Load modules.

;;; Code:

;; -----------------------------------------

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

(require 'dot-elpaca)
(require 'dot-setup-el)

(require 'dot-core)
(require 'dot-ui)
(require 'dot-evil)
(require 'dot-development)
(require 'dot-selection)
(require 'dot-org-mode)
(require 'dot-mail)
(require 'dot-rss)
(require 'dot-keybinds)
