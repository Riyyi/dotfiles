;;; dot-core-functions.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom elisp functions and macros.

;;; Code:

;; -----------------------------------------
;;  General Functions

;; Functions that only use built-in Emacs functionality.

(defun display-startup-echo-area-message ()
  "Hide default startup message."
  (message nil))

(defun dot/config-visit ()
  "Edit config file."
  (interactive)
  (find-file (expand-file-name "init.el" dot-emacs-dir)))

(defun dot/config-reload ()
  "Reload config file."
  (interactive)
  (load (expand-file-name "init.el" dot-emacs-dir)))

(defun dot/copy-cpp-function-implementation ()
  "Copy C++ function implementation to clipboard."
  (interactive)
  (save-excursion
    (let ((func (save-excursion
                  (re-search-backward "\\b")
                  (re-search-forward "\\([^;]+\\);")
                  (match-string 1)))
          (type (progn
                  (re-search-backward "\\b")
                  (push-mark)
                  (back-to-indentation)
                  (buffer-substring (mark) (point))))
          (class (progn
                   (backward-up-list)
                   (backward-sexp)
                   (back-to-indentation)
                   (forward-to-word 1)
                   (current-word))))
      (kill-new (concat type class "::" func "\n{\n}"))))
  (message "Copied function implementation"))

;; Reference: http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
(defun dot/directory-files-recursively-depth (dir regexp include-directories maxdepth)
  "Depth limited variant of the built-in `directory-files-recursively'."
  (let ((result '())
        (current-directory-list (directory-files dir t)))
    (dolist (path current-directory-list)
      (cond
       ((and (file-regular-p path)
             (file-readable-p path)
             (string-match regexp path))
        (setq result (cons path result)))
       ((and (file-directory-p path)
             (file-readable-p path)
             (not (string-equal "/.." (substring path -3)))
             (not (string-equal "/." (substring path -2))))
        (when (and include-directories
                   (string-match regexp path))
          (setq result (cons path result)))
        (when (> maxdepth 1)
          (setq result (append (nreverse (dot/directory-files-recursively-depth
                                          path regexp include-directories (- maxdepth 1)))
                               result))))
       (t)))
    (reverse result)))

(defun dot/dired-find-file ()
  "In Dired, visit the file or directory named on this line."
  (interactive)
  (if (file-directory-p (dired-file-name-at-point))
      (dired-find-alternate-file)
    (dired-find-file)))

(defun dot/dired-up-directory ()
  "Run Dired on parent directory of current directory."
  (interactive)
  (find-alternate-file ".."))

(defun dot/find-file-emacsd ()
  "Find file under `dot-emacs-dir', recursively."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name
                       (directory-files-recursively dot-emacs-dir ""))))
    (find-file (completing-read "Find file (emacs): " files nil t))))

(defun dot/indent-buffer ()
  "Indent each nonblank line in the buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun dot/insert-spaces-until-column (until-column)
  "Insert spaces from point to UNTIL-COLUMN."
  (interactive "nInsert spaces until column: ")
  (let ((current-column (current-column)))
    ;; Increment column if the index is 1 based
    (when (not column-number-indicator-zero-based)
      (setq current-column (+ current-column 1)))
    ;; Insert spaces
    (let ((diff (- until-column current-column)))
      (if (> diff 0)
          (save-excursion (insert (make-string diff ?\ )))
        (user-error "Column should be higher than point")))))

(defun dot/reload-theme ()
  "Reload custom theme."
  (interactive)
  (mapc 'load (file-expand-wildcards
               (concat (car custom-theme-load-path) "*.el")))
  (load-theme (car custom-enabled-themes) t))

(defun dot/sudo-find-file (filename)
  "Edit file FILENAME as root."
  (interactive "FOpen file (as root): ")
  (find-file (concat "/sudo:root@localhost:" filename)))

(defun dot/sudo-this-file ()
  "Edit the current file as root."
  (interactive)
  (if buffer-file-name
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (princ "Current buffer isn't a file")))

(defun dot/toggle-fringe (&optional arg)
  "Toggle left-only fringe, or set state with ARG."
  (interactive)
  (if (or (and (eq fringe-mode 0) (eq arg nil))
          (eq arg 1))
      (set-fringe-mode '(nil . 0))
    (set-fringe-mode 0)))

(defun dot/M-x (command)
  "Prompt and execute COMMAND."
  (interactive "CCommand: ")
  (command-execute command))

(defun split-follow-horizontally ()
  "Split and follow window."
  (interactive)
  (split-window-below)
  (other-window 1))
(defun split-follow-vertically ()
  "Split and follow window."
  (interactive)
  (split-window-right)
  (other-window 1))

;; https://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; -----------------------------------------
;; Hook call functions

(defun dot/hook-disable-line-numbers ()
  "Disable the line numbers."
  (display-line-numbers-mode 0))

(defun dot/hook-disable-mode-line ()
  "Disable the mode line."
  (setq-local mode-line-format nil))

(provide 'dot-core-functions)

;; -----------------------------------------
;;  Macros

;; Reference: https://github.com/arcticicestudio/nord-emacs/issues/59#issuecomment-414882071
(defmacro dot/run-after-new-frame (func)
  "Run FUNC once or after every frame creation.
This is needed for UI initialization when running with the daemon."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame) (with-selected-frame frame ,func)))
     ,func))

;;; dot-core-functions.el ends here
