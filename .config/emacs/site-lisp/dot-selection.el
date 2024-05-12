;;; dot-selection.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;  Incremental narrowing in Emacs.

;;; Code:

(elpaca-setup prescient
  (:require prescient)
  (:when-loaded
	(setq completion-styles '(prescient basic))
	(setq prescient-filter-method '(literal regexp fuzzy))
	(setq prescient-save-file (expand-file-name "prescient-save.el" dot-cache-dir))
	(prescient-persist-mode)))

(elpaca-setup (vertico :files (:defaults "extensions/*"))
  (:load-after prescient)
  (:when-loaded
	(setq vertico-previous-directory nil)
	(defun dot/vertico-backspace ()
      "In Vertico file completion, backward kill sexp, delete char otherwise."
      (interactive)
      (if (not (and (active-minibuffer-window)
					minibuffer-completing-file-name))
          (backward-delete-char 1)
		(setq vertico-previous-directory
			  (concat (file-name-nondirectory (directory-file-name (minibuffer-contents)))
					  "/"))
        (vertico-directory-delete-word 1)))

	(vertico-mode))

  (defun dot/vertico-dired-goto-last-visited (&rest _)
	"Go to directory candidate that was last visited."
	(when minibuffer-completing-file-name
      (setq vertico--index (or (seq-position vertico--candidates vertico-previous-directory)
                               (when (string= vertico-previous-directory "~/")
                                 (seq-position vertico--candidates (concat user-login-name "/")))
                               (if (= vertico--total 0) -1 0)))
	  (setq vertico-previous-directory nil)))
  (advice-add 'vertico--update-candidates :after #'dot/vertico-dired-goto-last-visited))

(elpaca-nil (setup vertico-mouse ; vertico-mouse.el is part of vertico
	   (:load-after vertico)
	   (:when-loaded (vertico-mouse-mode))))

(elpaca-setup vertico-prescient
  (:load-after vertico prescient)
  (:when-loaded
    (vertico-prescient-mode)))

(elpaca-setup marginalia
  (:load-after vertico)
  (:when-loaded
	(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
	(marginalia-mode)))

(elpaca-setup consult
  (:load-after vertico)
  (:when-loaded
    (setq consult-narrow-key (kbd "?"))

	(defun dot/consult-line-no-fuzzy ()
      "Call consult-line without fuzzy matching."
      (interactive)
	  (let ((prescient-filter-method (delete 'fuzzy (copy-tree prescient-filter-method))))
        (consult-line)))

    (consult-customize
     dot/consult-line-no-fuzzy :prompt "Search: "
     consult-line-multi :prompt "Search: " :initial nil)))

(elpaca-setup consult-flycheck
  (:load-after consult flycheck))

(elpaca-setup consult-project-extra
  (:load-after consult project)
  (:when-loaded (setq project-switch-commands 'consult-project-extra-find)))

(provide 'dot-selection)

;;; dot-selection.el ends here
