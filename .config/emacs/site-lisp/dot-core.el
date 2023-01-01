;;; dot-core.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Load configuration core.

;;; Code:

;; -----------------------------------------

(add-to-list 'load-path (locate-user-emacs-file "site-lisp/core"))

(require 'dot-core-variables)
(require 'dot-core-customize)
(require 'dot-core-fonts)
(require 'dot-core-packages)
(require 'dot-core-config)
(require 'dot-core-functions)
(require 'dot-core-advice)
(require 'dot-core-hooks)

(provide 'dot-core)

;;; dot-core.el ends here
