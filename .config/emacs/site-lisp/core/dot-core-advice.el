;;; dot-core-advice.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Advice and Aliases.

;;; Code:

;; -----------------------------------------

;;  Advice

;; Define default terminal option.
(defun dot/ansi-term (program &optional new-buffer-name)
  (interactive (list dot/shell)))
(advice-add 'ansi-term :before #'dot/ansi-term)

;;  Aliases

;; Make confirm easier, by just pressing y/n.
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'dot-core-advice)

;;; dot-core-advice.el ends here
