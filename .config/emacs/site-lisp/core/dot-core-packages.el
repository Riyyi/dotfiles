;;; dot-core-packages.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Install core packages.

;;; Code:

;;; Compile

;; Automatically compile all packages.
;; https://github.com/emacscollective/auto-compile

(elpaca-setup auto-compile
  (:require auto-compile)
  (:when-loaded
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;;; General packages

(elpaca-setup general
  (:load-after evil)
  (:when-loaded
    ;; Fix for issue: general #493 and evil #130, #301
    ;; https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately
    (defun dot/general-fix-leader-key ()
      "Fix leader key in *Messages* buffer."
      (when-let ((messages-buffer (get-buffer "*Messages*")))
        (with-current-buffer messages-buffer
          (evil-normalize-keymaps))))
    (add-hook 'emacs-startup-hook #'dot/general-fix-leader-key)))

(elpaca-setup avy)

(elpaca-setup hungry-delete
  (:require hungry-delete)
  (:when-loaded (global-hungry-delete-mode)))

(elpaca-setup smart-tabs-mode
  ;; TODO: how does this get auto-loaded?
  (:when-loaded
    (smart-tabs-add-language-support latex latex-mode-hook
      ((latex-indent-line . 4)
       (latex-indent-region . 4)))
    ;; FIXME: breaks for Python files
    (smart-tabs-insinuate 'c 'c++ 'java 'python 'latex)))

(elpaca-setup super-save
(:require super-save)
  (:when-loaded
  (setq super-save-auto-save-when-idle t)

  ;; Fix for issues: super-save #38 and lsp-mode #1322
  (defun dot/super-save-disable-advice (orig-fun &rest args)
 "Dont auto-save under these conditions."
 (unless (equal (car args) " *LV*")
   (apply orig-fun args)))
  (advice-add 'super-save-command-advice :around #'dot/super-save-disable-advice)

  (super-save-mode)))

(elpaca nil (setup desktop  ; built-in
       (:require desktop)
       (:when-loaded
         (setq desktop-base-file-name "state")
         (setq desktop-base-lock-name "state.lock")
         (setq desktop-dirname (expand-file-name "desktop/" dot-cache-dir))
         (setq desktop-path (list desktop-dirname))
         (setq desktop-globals-to-save '()) ;; Only need frames and buffers

         ;; Create directory to store desktop file in
         (unless (file-directory-p desktop-dirname)
           (make-directory desktop-dirname t))

         (defun dot/desktop-save ()
           "Save frame state and buffers."
           (interactive)
           (dot/centaur-tabs-buffer-cleanup)
           (desktop-save desktop-dirname t))

         (defun dot/desktop-save-on-exit ()
           "Save state of buffers before closing Emacs."
           (dot/desktop-save)
           (desktop-release-lock desktop-dirname))
         (add-hook 'kill-emacs-hook #'dot/desktop-save-on-exit))))

(provide 'dot-core-packages)

;;; dot-core-packages.el ends here
