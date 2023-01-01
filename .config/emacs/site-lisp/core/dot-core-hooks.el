;;; dot-hooks.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Add hooks.

;;; Code:

;; -----------------------------------------

;; Delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Display fill column indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

;; Highlight parenthesis
(add-hook 'prog-mode-hook #'show-paren-mode)

;; Disable line numbers
(add-hook 'Custom-mode-hook #'dot/hook-disable-line-numbers)
(add-hook 'dired-mode-hook #'dot/hook-disable-line-numbers)
(add-hook 'Info-mode-hook #'dot/hook-disable-line-numbers)
(add-hook 'term-mode-hook #'dot/hook-disable-line-numbers)

;; Wrap lines in the middle of words, gives a \ indicator
(add-hook 'visual-line-mode-hook (lambda () (setq word-wrap nil)))

(provide 'dot-core-hooks)

;;; dot-hooks.el ends here
