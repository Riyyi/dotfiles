;;; dot-evil.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;  Setup Evil and related packages.

;;; Code:

;; -----------------------------------------

(elpaca-setup undo-tree
  (:when-loaded
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" dot-cache-dir))))
    (global-undo-tree-mode)))

(elpaca-setup goto-chg)

(elpaca-setup evil
  (:also-load undo-tree goto-chg)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-operator-state-cursor 'box) ; Do not set half cursor
  (setq evil-search-module 'evil-search)
  (setq evil-split-window-below t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ; Needed by evil-collection
  (:require evil)
  (:when-loaded

    ;; Put search results in the center of the window
    (defun dot/evil-scroll-center (&rest _)
      "Scroll cursor to center of the window."
      (evil-scroll-line-to-center nil))
    (advice-add 'evil-ex-search :after #'dot/evil-scroll-center)
    (advice-add 'evil-match :after #'dot/evil-scroll-center)

    (defun dot/evil-normal-sort-paragraph ()
      "Sort paragraph cursor is under.
Vim equivalence: vip:sort<CR>"
      (interactive)
      (let ((p (point)))
        (evil-visual-char)
        (call-interactively 'evil-inner-paragraph)
        (evil-ex-sort (region-beginning) (region-end))
        (goto-char p)))

    (defun dot/evil-insert-shift-left ()
      "Shift line left, retains cursor position.
Vim equivalence: <C-D>"
      (interactive)
      (evil-shift-left-line 1))

    (defun dot/evil-insert-shift-right ()
      "Shift line right, retains cursor position.
Vim equivalence: <Tab>"
      (interactive)
      (insert "\t"))

    (defun dot/evil-visual-shift-left ()
      "Shift visual selection left, retains the selection.
Vim equivalence: <gv"
      (interactive)
      (call-interactively #'evil-shift-left)
      (setq deactivate-mark nil))

    (defun dot/evil-visual-shift-right ()
      "Shift visual selection left, retains the selection.
Vim equivalence: >gv"
      (interactive)
      (call-interactively #'evil-shift-right)
      (setq deactivate-mark nil))

    (evil-mode)))

;; Evil command aliases.

(elpaca nil (setup evil-ex ; evil-ex.el is part of evil
       (:when-loaded
         (evil-ex-define-cmd "W" "w")
         (evil-ex-define-cmd "Q" "q")
         (evil-ex-define-cmd "WQ" "wq")
         (evil-ex-define-cmd "Wq" "wq"))))

(elpaca-setup evil-collection
  (setq evil-collection-company-use-tng nil)
  (setq evil-collection-key-blacklist (list dot/leader-key dot/localleader-key
                                            dot/leader-alt-key dot/localleader-alt-key
                                            "M-h" "M-j" "M-k" "M-l"))
  (setq evil-collection-setup-minibuffer t)
  (:load-after evil)
  (:when-loaded
    (evil-collection-init)))

(elpaca-setup evil-nerd-commenter
  (:load-after evil))

(provide 'dot-evil)

;;; dot-evil.el ends here
