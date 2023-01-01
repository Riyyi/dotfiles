;;; dot-core-customize.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; ??

;;; Code:

;; -----------------------------------------
;; Customizations

;; Store customize file separately, don't freak out when it's not found.

(setq custom-file (expand-file-name "custom.el" dot-etc-dir))
(load custom-file 'noerror)

(provide 'dot-core-customize)

;;; dot-core-customize.el ends here
