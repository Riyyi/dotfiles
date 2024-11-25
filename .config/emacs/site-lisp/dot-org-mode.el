;;; dot-org-mode.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Load org-mode modules.

;;; Code:

;; -----------------------------------------
;; LaTeX Configuration

(elpaca-nil (setup tex-mode ; built-in
       (:when-loaded
         (defun dot/tex-mode-init () ""
                (setq indent-tabs-mode t)
                (setq tab-width 4)
                (setq tex-indent-basic 4))
         (:hook dot/tex-mode-init)

         (with-eval-after-load 'project
           (defun compile-latex ()
             "Compile LaTeX project."
             (interactive)
             (let ((default-directory (dot/find-project-root)))
               (dot/project-save-project-buffers)
               (shell-command "make")))))))

;; -----------------------------------------
;; Org Configuration

;; Base Org.

(elpaca-nil (setup org ; built-in
  (setq org-directory (expand-file-name "documents/org" (getenv "HOME")))
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  (:with-mode before-save
    (:hook dot/org-set-last-modified))
  (:when-loaded
    (setq org-adapt-indentation nil)
    (setq org-ellipsis "⤵")
    (setq org-image-actual-width nil)
    (setq org-startup-folded nil)

    ;; Enable structured template completion
    (add-to-list 'org-modules 'org-tempo t)
    (add-to-list 'org-structure-template-alist
                 '("el" . "src emacs-lisp"))

    (defun dot/org-find-or-create-heading (heading level)
      "Find or create heading"
      (if (re-search-forward (format org-complex-heading-regexp-format
                                     (regexp-quote heading))
                             nil t)
          (beginning-of-line)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (concat (make-string level ?*) " ") heading "\n")
        (beginning-of-line 0)))

    (defun dot/org-find-month-week-day ()
      "Find or create the journal tree for the current week under current month"
      (unless (derived-mode-p 'org-mode)
        (error "Target buffer \"%s\" for dot/find-org-week should be in Org mode"
               (current-buffer)))
      (org-capture-put-target-region-and-position)
      (widen)
      (goto-char (point-min))
      (dot/org-find-or-create-heading "Worklog" 1)
      (org-narrow-to-subtree)
      (dot/org-find-or-create-heading (format-time-string "%B") 2) ;; month
      (org-narrow-to-subtree)
      (dot/org-find-or-create-heading (format-time-string "Week %W") 3)
      (org-narrow-to-subtree)
      (dot/org-find-or-create-heading (format-time-string "%A") 4) ;; day
      (org-end-of-subtree))

    ;; Capture templates
    (setq org-capture-templates
          `(("i" "Clock in" plain (file+function ,(expand-file-name "worklog.org" org-directory) dot/org-find-month-week-day)
             "| %<%Y-%m-%d> | IN | %<%H:%M> | %^{Location|Office|Home|Office|Visit} |\n===================================%?"
             :immediate-finish t)
            ("o" "Clock out" plain (file+function ,(expand-file-name "worklog.org" org-directory) dot/org-find-month-week-day)
             "===================================\n| %<%Y-%m-%d> | OUT | %<%H:%M> |%?"
             :immediate-finish t)
            ("s" "Switch task" plain (file+function ,(expand-file-name "worklog.org" org-directory) dot/org-find-month-week-day)
             "| %<%Y-%m-%d> | %<%H:%M> | %^{Item ID} | X | %^{Description|-} |%?"
             :immediate-finish t)))

    ;; Keybind functions
    (defun dot/org-clock-in () (interactive) (org-capture nil "i"))
    (defun dot/org-clock-out () (interactive) (org-capture nil "o"))
    (defun dot/org-switch-task () (interactive) (org-capture nil "s"))

    (with-eval-after-load 'evil-commands
      (defun dot/org-ret-at-point ()
        "Org return key at point.

If point is on:
  checkbox   -- toggle it
  link       -- follow it
  table      -- go to next row
  otherwise  -- run the default (evil-ret) expression"
        (interactive)
        (let ((type (org-element-type (org-element-context))))
          (pcase type
            ('link (if org-return-follows-link (org-open-at-point) (evil-ret)))
            ((guard (org-at-item-checkbox-p)) (org-toggle-checkbox))
            ('table-cell (org-table-next-row))
            (_ (evil-ret))
            ))))

  (defun dot/org-find-file-property (property &optional anywhere)
    "Return the position of the file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun dot/org-set-file-property (property value)
    "Set the file PROPERTY in the preamble."
    (when-let ((pos (dot/org-find-file-property property)))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (insert value))))

  (defun dot/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (dot/org-set-file-property "LAST_MODIFIED"
                                 (format-time-string "[%Y-%m-%d %a %H:%M]")))))))

;; Org agenda.

(elpaca-nil (setup org-agenda ; built-in
       (:load-after evil org)
       (:when-loaded
         (setq org-agenda-files `(,org-directory ,user-emacs-directory))
         (setq org-agenda-span 14)
         (setq org-agenda-window-setup 'current-window)
         (evil-set-initial-state 'org-agenda-mode 'motion))))

;; Org capture.

(elpaca-nil (setup org-capture ; built-in
       ;; Org-capture in new tab, rather than split window
       (:hook delete-other-windows)))

;; Org keys.

(elpaca-nil (setup org-keys ; built-in
       (:when-loaded
         (setq org-return-follows-link t))))

;; Org links.

(elpaca-nil (setup ol ; built-in
       (:when-loaded
         ;; Do not open links to .org files in a split window
         (add-to-list 'org-link-frame-setup '(file . find-file)))))

;; Org source code blocks.

(elpaca-nil (setup org-src ; built-in
       (:when-loaded
         (setq org-edit-src-content-indentation 0)
         (setq org-src-fontify-natively t)
         (setq org-src-preserve-indentation t)
         (setq org-src-tab-acts-natively t)
         (setq org-src-window-setup 'current-window))))

;; Org exporter.

(elpaca-nil (setup ox ; built-in
       (:when-loaded
         (setq org-export-coding-system 'utf-8-unix))))

;; Org latex exporter.

(elpaca-nil (setup ox-latex ; built-in
       (:when-loaded
         ;; Define how minted (highlighted src code) is added to src code blocks
         ;; Requires packages: texlive-bin, texlive-latexextra and python-pygments
         (setq org-latex-listings 'minted)
         (setq org-latex-minted-options '(("frame" "lines") ("linenos=true")))
         ;; Set 'Table of Contents' layout
         (setq org-latex-toc-command "\\newpage \\tableofcontents \\newpage")
         ;; Add minted package to every LaTeX header
         (add-to-list 'org-latex-packages-alist '("" "minted"))
         ;; Add -shell-escape so pdflatex exports minted correctly
         (setcar org-latex-pdf-process (replace-regexp-in-string
                                        "-%latex -interaction"
                                        "-%latex -shell-escape -interaction"
                                        (car org-latex-pdf-process))))))

;;; Org Bullets

(elpaca-setup org-bullets
  (:hook-into org-mode))

;;; Org Export Packages

;; HTML exporter.

(elpaca-setup htmlize
  (:when-loaded (setq org-export-html-postamble nil)))
;;org-export-html-postamble-format ; TODO

;; GitHub flavored Markdown exporter.

(elpaca-setup ox-gfm)

;;; Org Roam

(elpaca-setup emacsql-sqlite-builtin)

(elpaca-setup org-roam
  (:autoload org-roam-node-find) ;; TODO, is this enough?
  (setq org-roam-v2-ack t)
  (setq org-roam-database-connector 'sqlite-builtin)
  (:when-loaded
    (setq org-roam-db-location (expand-file-name "org-roam.db" dot-cache-dir))
    (setq org-roam-directory org-directory)
    ;; Exclude Syncthing backup directory
    (setq org-roam-file-exclude-regexp "\\.stversions")
    (setq org-roam-verbose nil)

    (setq org-roam-capture-templates
          '(("d" "default" plain
             "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+LAST_MODIFIED:\n#+FILETAGS: %^{File tags||:structure:}\n")
             :unnarrowed t)))

    (defun dot/org-roam-node-insert-immediate (arg &rest args)
      (interactive "P")
      (let ((args (push arg args))
            (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                      '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

    (cl-defmethod org-roam-node-slug ((node org-roam-node))
      "Return the slug of NODE, strip out common words."
      (let* ((title (org-roam-node-title node))
             (words (split-string title " "))
             (common-words '("a" "an" "and" "as" "at" "by" "is" "it" "of" "the" "to"))
             (title (string-join (seq-remove (lambda (element) (member element common-words)) words) "_"))
             (pairs '(("c\\+\\+" . "cpp")             ;; convert c++ -> cpp
                      ("c#" . "cs")                   ;; convert  c# -> cs
                      ("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                      ("__*" . "_")                   ;; remove sequential underscores
                      ("^_" . "")                     ;; remove starting underscore
                      ("_$" . ""))))                  ;; remove ending underscore
        (cl-flet ((cl-replace (title pair)
                              (replace-regexp-in-string (car pair) (cdr pair) title)))
          (downcase (-reduce-from #'cl-replace title pairs)))))

    ;; Right-align org-roam-node-tags in the completion menu without a length limit
    ;; Source: https://github.com/org-roam/org-roam/issues/1775#issue-971157225
    (setq org-roam-node-display-template "${title} ${tags:0}")
    (setq org-roam-node-annotation-function #'dot/org-roam-annotate-tag)
    (defun dot/org-roam-annotate-tag (node)
      (let ((tags (mapconcat 'identity (org-roam-node-tags node) " #")))
        (unless (string-empty-p tags)
          (concat
           (propertize " " 'display `(space :align-to (- right ,(+ 2 (length tags)))))
           (propertize (concat "#" tags) 'face 'bold)))))

    (org-roam-setup)))

;; Enable https://www.orgroam.com/manual.html#org_002droam_002dprotocol, needed to process org-protocol:// links

(elpaca-nil (setup org-roam-protocol ; org-roam-protocol.el is part of org-roam
       (:load-after org-roam)
       (:when-loaded

         ;; Templates used when creating a new file from a bookmark
         (setq org-roam-capture-ref-templates
               '(("r" "ref" plain
                  "%?"
                  :target (file+head "${slug}.org" "#+TITLE: ${title}\n \n${body}")
                  :unnarrowed t))))))

;; The roam-ref protocol bookmarks to add:

;; javascript:location.href =
;;  'org-protocol://roam-ref?template=r'
;;  + '&ref=' + encodeURIComponent(location.href)
;;  + '&title=' + encodeURIComponent(document.title)
;;  + '&body=' + encodeURIComponent(window.getSelection())

;; Setup org-roam-ui, runs at http://127.0.0.1:35901.

(elpaca-setup org-roam-ui
  (:load-after org-roam)
  (:when-loaded
    (setq org-roam-ui-follow t)
    (setq org-roam-ui-open-on-start t)
    (setq org-roam-ui-sync-theme nil) ;; FIXME: Make this work (org-roam-ui-get-theme)
    (setq org-roam-ui-update-on-save t)))

;; Easily searchable .org files via Deft.

(elpaca-setup deft
  (:hook dot/hook-disable-line-numbers)
  (:when-loaded
    (setq deft-auto-save-interval 0)
    (setq deft-default-extension "org")
    (setq deft-directory org-directory)
    (setq deft-file-naming-rules '((noslash . "-")
                                   (nospace . "-")
                                   (case-fn . downcase)))
    (setq deft-new-file-format "%Y%m%d%H%M%S-deft")
    (setq deft-recursive t)
    ;; Exclude Syncthing backup directory
    (setq deft-recursive-ignore-dir-regexp (concat "\\.stversions\\|" deft-recursive-ignore-dir-regexp))
    ;; Remove file variable -*- .. -*- and Org Mode :PROPERTIES: lines
    (setq deft-strip-summary-regexp (concat "\\(^.*-\\*-.+-\\*-$\\|^:[[:alpha:]_]+:.*$\\)\\|" deft-strip-summary-regexp))
    (setq deft-use-filename-as-title nil)
    (setq deft-use-filter-string-for-filename t)

    (add-to-list 'deft-extensions "tex")

    ;; Start filtering immediately
    (with-eval-after-load 'evil
      (evil-set-initial-state 'deft-mode 'insert))

    (defun deft-parse-title (file contents)
      "Parse the given FILE and CONTENTS and determine the title."
      (if (string-match "#\\+\\(TITLE\\|title\\):\s*\\(.*\\)$" contents)
          (match-string 2 contents)
        (deft-base-filename file)))))

;;; Org "Table of Contents"

;; Generate table of contents without exporting.
(elpaca-setup toc-org
  (:hook-into org-mode))

(provide 'dot-org-mode)

;;; dot-org-mode.el ends here
