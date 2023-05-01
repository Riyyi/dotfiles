;;; dot-core-variables.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Global variables.

;;; Code:

;; -----------------------------------------
;; Global Variables

;; Variables for directories, leader keys, etc.

(defvar dot-emacs-dir (directory-file-name (file-truename user-emacs-directory))
  "Directory base.") ; ~/.config/emacs

(defvar dot-etc-dir (expand-file-name "etc" dot-emacs-dir)
  "Directory for non-volatile storage.") ; ~/.config/emacs/etc

(defvar dot-cache-dir
  (expand-file-name "emacs" (if (getenv "XDG_CACHE_HOME") (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for cache data.") ; ~/.cache/emacs

(defvar dot/leader-key "SPC"
  "Leader prefix key.")

(defvar dot/leader-alt-key "M-SPC"
  "Alternative leader prefix key, used for Insert and Emacs states.")

(defvar dot/localleader-key "SPC m"
  "Local leader prefix key, for 'major-mode' specific commands.")

(defvar dot/localleader-alt-key "M-SPC m"
  "Alternative local leader prefix key, used for Insert and Emacs states.")

(defvar dot/shell "/bin/zsh"
  "Command interpreter binary path.")

(defvar dot/hidpi (getenv "HIDPI")
  "Whether the primary screen is HiDPI.")

;; Create cache directory
(unless (file-directory-p dot-cache-dir)
  (make-directory dot-cache-dir t))

(provide 'dot-core-variables)

;;; dot-core-variables.el ends here
