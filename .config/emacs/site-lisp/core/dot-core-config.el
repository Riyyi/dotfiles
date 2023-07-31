;;; dot-core-config.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; ??

;;; Code:

;; -----------------------------------------
;; General

;; Columns start at 1
(setq column-number-indicator-zero-based nil)
;; TODO: Make variable below compatible with telephone-line
;; (setq mode-line-position-column-format " C%C")

;; Dont confirm on quitting Emacs
(setq confirm-kill-processes nil)

;; Custom thems, do not ask if safe
(setq custom-safe-themes t)

;; Dired move to trash
(setq delete-by-moving-to-trash t)

;; Column indicator character
(setq display-fill-column-indicator-character ?\N{U+2503})

;; Scrolling
(setq scroll-conservatively 1)
(setq mouse-wheel-scroll-amount '(5))
(setq mouse-wheel-progressive-speed nil)

;; Parenthesis, set behavior
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(setq show-paren-context-when-offscreen t)

;; Tramp default protocol
(setq tramp-default-method "ssh")

;; Set undo limit, measured in bytes
(setq-default undo-limit 400000)
(setq-default undo-strong-limit 3000000)
(setq-default undo-outer-limit 12000000)

;; Enable line numbers
(global-display-line-numbers-mode)

;; C++ syntax highlighting for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Set the frame title
(setq frame-title-format
      `("%b"
        (:eval
         (if (buffer-file-name)
             (concat
              (if (buffer-modified-p) " •" nil)
              " ("
              (abbreviate-file-name
               (directory-file-name
                (file-name-directory (buffer-file-name))))
              ")")
           nil))
        ,(format " - GNU Emacs %s" emacs-version)
        ))
(setq icon-title-format frame-title-format)

;; -----------------------------------------
;; Buffers

(setq confirm-nonexistent-file-or-buffer nil)
(setq ibuffer-expert t)

;; -----------------------------------------
;; Dired

(setq wdired-allow-to-change-permissions t)

;; -----------------------------------------
;; Electric

;; Make return key also do indent of previous line
(electric-indent-mode 1)
(setq electric-pair-pairs '(
                            (?\( . ?\))
                            (?\[ . ?\])
                            ))
(electric-pair-mode 1)

;; -----------------------------------------
;; File Paths

;; Set file paths for built-in features like: auto-saves, backups, etc.

;; Set Directory locations
(setq auto-save-list-file-prefix      (expand-file-name "auto-save/" dot-cache-dir))
(setq auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t)))
(setq backup-directory-alist         `((".*" . ,(expand-file-name "backup/" dot-cache-dir))))
(setq custom-theme-directory          (expand-file-name "themes/" dot-emacs-dir))
(setq eshell-directory-name           (expand-file-name "eshell/" dot-cache-dir))
(setq tramp-auto-save-directory       (expand-file-name "tramp-auto-save/" dot-cache-dir))
(setq tramp-backup-directory-alist    backup-directory-alist)
(setq treesit-extra-load-path        `(,(expand-file-name "tree-sitter" dot-cache-dir)))
(setq url-configuration-directory     (expand-file-name "url/" dot-cache-dir))

(startup-redirect-eln-cache (expand-file-name "eln-cache" dot-cache-dir))

;; Set file locations
(setq bookmark-default-file           (expand-file-name "bookmarks" dot-etc-dir))
(setq nsm-settings-file               (expand-file-name "network-security.data" dot-cache-dir))
(setq org-id-locations-file           (expand-file-name "org-id-locations" dot-cache-dir))
(setq tramp-persistency-file-name     (expand-file-name "tramp" dot-cache-dir ))

;; -----------------------------------------
;; File Backups Versioning

;; Setup file backups versioning.

(setq backup-by-copying t)    ; Don't cobbler symlinks
(setq create-lockfiles nil)   ; Disable lockfiles (.#)
(setq delete-old-versions t)  ; Cleanup backups
(setq kept-new-versions 5)    ; Newest backups to keep
(setq kept-old-versions 2)    ; Oldest backups to keep
(setq version-control t)      ; Use version numbers on backups

;; -----------------------------------------
;; Formatting

;; Columnn after line-wrapping happens
(setq-default fill-column 80)

;; Automatically add newline on save at the end of the file
(setq require-final-newline t)

;; End sentences with a single space
(setq sentence-end-double-space nil)

;; `tabify' and `untabify' should only affect indentation
(setq tabify-regexp "^\t* [ \t]+")

;; Do not wrap lines
(setq-default truncate-lines t)

;; Wrap lines in the middle of words, gives a \ indicator
(setq-default word-wrap nil)

;; -----------------------------------------
;; Hide Elements

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(fringe-mode 0)
(blink-cursor-mode 0)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

;; -----------------------------------------
;; Native Compilation

(setq native-comp-async-report-warnings-errors nil)

;; -----------------------------------------
;; Recentf

(elpaca nil (setup recentf ; built-in
       (:require recentf)
       (:when-loaded
         (setq recentf-auto-cleanup 'never)
         (setq recentf-exclude '("~$" "/ssh:" "/sudo:"))
         (setq recentf-filename-handlers '(abbreviate-file-name))
         (setq recentf-max-menu-items 0)
         (setq recentf-max-saved-items 200)
         (setq recentf-save-file (expand-file-name "recentf" dot-cache-dir))
         (recentf-mode))))

;; -----------------------------------------
;; Tabs

;; Tabs
(setq-default tab-width 4
              indent-tabs-mode t
              c-basic-offset 4
              sgml-basic-offset 4
              sh-basic-offset 4)

;; C/C++-like languages formatting style
;; https://www.emacswiki.org/emacs/IndentingC
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Getting-Started
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Adding-Styles
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Sample-Init-File
(c-add-style "user" `("linux"
                      (c-basic-offset . ,(default-value 'tab-width))
                      (c-offsets-alist
                       (innamespace . -)
                       )))
(setq-default c-default-style "user")

;; -----------------------------------------
;; UTF-8

;; Set UTF-8 encoding as default.

(prefer-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
;; Default also sets file-name, keyboard and terminal coding system
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)

;; -----------------------------------------
;; Window

;; Set `switch-to-buffer' to respect the window rules
(setq switch-to-buffer-obey-display-actions t)

;; Window rules
(setq display-buffer-alist
      '(
        ;; ^\*(e?shell|(ansi-|v)?term).*
        ("^\\*\\(e?shell\\|\\(ansi-\\|v\\)?term\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . -1))
        ("\\*Faces\\*"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . 1))
        ("\\*Help.*"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . 0))
        ))

;; Allow 'undo' and 'redo' changes in Window Configuration.
(winner-mode)

(provide 'dot-core-config)

;;; dot-core-config.el ends here
