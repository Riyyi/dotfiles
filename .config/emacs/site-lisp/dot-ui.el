;;; dot-ui.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;  Setup UI packages.

;;; Code:

;; -----------------------------------------
;; All the icons

(elpaca-setup all-the-icons
  (:require all-the-icons)
  (:when-loaded

    ;; Install all-the-icons if font files are not found
    (dot/run-after-new-frame
     (unless (find-font (font-spec :name "all-the-icons"))
       (all-the-icons-install-fonts t)))))

(elpaca-setup all-the-icons-dired
  (:hook-into dired-mode)
  (:load-after all-the-icons)
  (:when-loaded (setq all-the-icons-dired-monochrome nil)))

;; -----------------------------------------
;; Centaur tabs

;; Places buffers as tabs in a bar at the top of the frame.

(elpaca-setup centaur-tabs
  (:load-after all-the-icons)
  (:with-mode
      (eshell-mode
       help-mode
       helpful-mode
       mu4e-view-mode
       neotree-mode
       shell-mode)
    (:hook centaur-tabs-local-mode))
  (:when-loaded
    (setq centaur-tabs-enable-ido-completion nil)
    (setq centaur-tabs-height (if dot/hidpi 38 18))
    (setq centaur-tabs-modified-marker "•")
    (setq centaur-tabs-set-icons t)
    (setq centaur-tabs-set-modified-marker t)
    (setq centaur-tabs-style "slant")

    (setq centaur-tabs-project-buffer-group-calc nil)
    (defun centaur-tabs-buffer-groups ()
      "Organize tabs into groups by buffer."
      (unless centaur-tabs-project-buffer-group-calc
        (set (make-local-variable 'centaur-tabs-project-buffer-group-calc)
             (list
              (cond
               ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
               ((or (memq major-mode '(magit-process-mode
                                       magit-status-mode
                                       magit-diff-mode
                                       magit-log-mode
                                       magit-file-mode
                                       magit-blob-mode
                                       magit-blame-mode))
                    (string= (buffer-name) "COMMIT_EDITMSG")) "Magit")
               ((project-current) (dot/project-project-name))
               ((memq major-mode '(org-mode
                                   emacs-lisp-mode)) "Org Mode")
               ((derived-mode-p 'dired-mode) "Dired")
               ((derived-mode-p 'prog-mode
                                'text-mode) "Editing")
               (t "Other")))))
      (symbol-value 'centaur-tabs-project-buffer-group-calc))

    (defun centaur-tabs-hide-tab (buffer)
      "Hide from the tab bar by BUFFER name."

      (let ((name (format "%s" buffer)))
        (or
         ;; Current window is dedicated window
         (window-dedicated-p (selected-window))
         ;; Buffer name matches below blacklist
         (string-match-p "^ ?\\*.*\\*" name))))

    (defun dot/centaur-tabs-is-buffer-unimportant (buffer)
      "Return t if BUFFER is unimportant and can be killed without caution."
      (let ((name (format "%s" buffer)))
        (cond
         ((centaur-tabs-hide-tab name) t)
         ((string-match-p "^magit\\(-[a-z]+\\)*: .*" name) t)
         (t nil))))

    (defun dot/centaur-tabs-buffer-cleanup ()
      "Clean up all the hidden buffers."
      (interactive)
      (dolist (buffer (buffer-list))
        (when (dot/centaur-tabs-is-buffer-unimportant buffer)
          (kill-buffer buffer)))
      (princ "Cleaned buffers"))

    (defun dot/centaur-tabs-kill-buffer-or-window ()
      "Delete window of the current buffer, also kill if the buffer is hidden."
      (interactive)
      (if (dot/centaur-tabs-is-buffer-unimportant (buffer-name))
          (kill-buffer-and-window)
        (delete-window)))

    (centaur-tabs-headline-match)
    (centaur-tabs-mode)))

;; -----------------------------------------
;; Dashboard

(elpaca-setup page-break-lines
  (:require page-break-lines))

(elpaca-setup dashboard
  (:require dashboard)
  (:hook dot/hook-disable-line-numbers)
  (:when-loaded
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*") (dashboard-refresh-buffer)))
    (setq dashboard-banner-logo-title "GNU Emacs master race!")
    (setq dashboard-center-content t)
    (setq dashboard-page-separator "\n\f\n")
    (setq dashboard-projects-backend 'project-el)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-footer nil)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-show-shortcuts t)
    (setq dashboard-startup-banner 'logo)
    (setq dashboard-items '((projects . 10)
                            (bookmarks . 5)
                            (recents . 5)))

    ;; Fix keybinds..

    (defun dot/dashboard-goto-bookmarks ()
      "Move point to bookmarks."
      (interactive)
      (funcall (local-key-binding "m")))

    (defun dot/dashboard-goto-projects ()
      "Move point to projects."
      (interactive)
      (funcall (local-key-binding "p")))

    (defun dot/dashboard-goto-recent-files ()
      "Move point to recent files."
      (interactive)
      (funcall (local-key-binding "r")))

    (dashboard-setup-startup-hook)
    ))

;; -----------------------------------------
;; Helpful

;; A better *help* buffer.

(elpaca-setup helpful
  (:require helpful)
  (:hook dot/hook-disable-line-numbers))

;; -----------------------------------------
;; Neotree

;; Provides Emacs with a file tree.

(elpaca-setup neotree
  (:autoload neotree-toggle)
  (:hook dot/hook-disable-line-numbers)
  (:hook hl-line-mode)

  ;; This needs to be in init to actually start loading the package
  (with-eval-after-load 'project
    (defun neotree-toggle-in-project-root ()
      "Toggle Neotree in project root."
      (interactive)
      (let ((default-directory (dot/find-project-root)))
        (call-interactively #'neotree-toggle))))

  (:when-loaded
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq neo-autorefresh nil)
    (setq neo-mode-line-type 'none)
    (setq neo-show-hidden-files t)
    (setq neo-vc-integration '(face))))

;; -----------------------------------------
;; Telephone Line

;; Emacs mode line replacement.

(elpaca-setup telephone-line
  (:require telephone-line)
  (:when-loaded
    (setq telephone-line-height (if dot/hidpi 30 15))
    (setq telephone-line-lhs
          '((evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment
                       telephone-line-buffer-segment))
            (nil    . (telephone-line-project-segment))))
    (telephone-line-mode)))

;; -----------------------------------------
;; Theme

(elpaca-setup hybrid-reverse-theme
  (:require hybrid-reverse-theme)
  (:when-loaded
    (dot/run-after-new-frame
     (load-theme 'hybrid-reverse t))))

;; -----------------------------------------
;; Which-key

;; Popup that displays available key bindings.

(elpaca-setup which-key
  (:hook-into elpaca-after-init)
  (:when-loaded
    (setq which-key-add-column-padding 1)
    (setq which-key-max-display-columns nil)
    (setq which-key-min-display-lines 6)
    (setq which-key-sort-order #'dot/which-key-prefix-then-key-order-alpha)
    (setq which-key-sort-uppercase-first nil)

    (defun dot/which-key-prefix-then-key-order-alpha (acons bcons)
      "Order by prefix, then lexicographical."
      (let ((apref? (which-key--group-p (cdr acons)))
            (bpref? (which-key--group-p (cdr bcons))))
        (if (not (eq apref? bpref?))
            (and (not apref?) bpref?)
          (which-key-key-order-alpha acons bcons))))))

(provide 'dot-ui)

;;; dot-ui.el ends here
