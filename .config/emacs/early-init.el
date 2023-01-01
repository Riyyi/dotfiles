;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduces early-init.el, which is run before package and UI
;; initialization happens.

;;; Code:

;; (setq debug-on-error t)

;; Defer the garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 8000000)))

;; -------------------------------------

;; Base directory
(setq user-emacs-directory (file-name-directory load-file-name))

;; -------------------------------------

;; Prefer to `load' the newest elisp file
(setq load-prefer-newer t)

;; Disable package.el
(setq package-enable-at-startup nil)

;; -------------------------------------

;; Disable frame bars before they're loaded in
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Do not resize the frame
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
