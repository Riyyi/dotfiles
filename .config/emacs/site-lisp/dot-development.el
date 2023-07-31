;;; dot-development.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Development.

;;; Code:

;; -----------------------------------------
;; Company

(elpaca-setup company
  (:require company)
  (:with-mode
      (c-mode-common
       emacs-lisp-mode
       latex-mode
       org-mode
       php-mode
       shell-mode
       shell-script-mode)
    (:hook company-mode))
  (:when-loaded
    (setq company-idle-delay 0.2)
    (setq company-minimum-prefix-length 2)
    (setq company-show-numbers t)
    (setq company-tooltip-align-annotations 't)))

;; Sort Company completions.

(elpaca-setup company-prescient
  (:load-after company prescient)
  (:when-loaded (company-prescient-mode)))

;; Auto-completion for C/C++ headers.

(elpaca-setup company-c-headers
  (:when-loaded (add-to-list 'company-backends 'company-c-headers)))

;; GLSL integration with company requires the package: ~glslang~.

(when (executable-find "glslangValidator")
  (elpaca-setup company-glsl
    (:when-loaded (add-to-list 'company-backends 'company-glsl))))

;; -----------------------------------------
;; Git

(elpaca-setup diff-hl
  (:require diff-hl)
  (:with-mode prog-mode
    (:hook turn-on-diff-hl-mode)
    (:hook dot/diff-hl-enable-flydiff-and-fringe))
  (:when-loaded

    (defun dot/diff-hl-enable-flydiff-and-fringe ()
      "Enable on the fly diff checking if file is under version control."
      (let ((buffer buffer-file-name))
        (when (and buffer (vc-registered buffer))
          (diff-hl-flydiff-mode)
          (dot/toggle-fringe 1))))))

(elpaca-setup transient
  (:when-loaded
    (setq transient-history-file (expand-file-name "transient/history.el" dot-cache-dir))
    (setq transient-values-file  (expand-file-name "transient/values.el"  dot-cache-dir))))

(elpaca-setup magit
  (:autoload magit-status magit-status-here)
  (:with-mode git-commit-setup
    (:hook git-commit-turn-on-auto-fill)
    (:hook git-commit-turn-on-flyspell))
  (:with-mode magit-pre-refresh (:hook diff-hl-magit-pre-refresh))
  (:with-mode magit-post-refresh (:hook diff-hl-magit-post-refresh))
  (:load-after diff-hl transient)
  (:when-loaded
    (setq git-commit-fill-column 72)
    (setq git-commit-summary-max-length 72)
    (setq magit-completing-read-function #'completing-read)
    (setq magit-diff-paint-whitespace-lines 'both)
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-process-extreme-logging nil)
    (setq magit-repository-directories '(("~/dotfiles" . 0)
                                         ("~/code" . 3)))

    (put 'magit-log-select-pick :advertised-binding [?\M-c])
    (put 'magit-log-select-quit :advertised-binding [?\M-k])

    (defun dot/magit-select-repo ()
      "Select project repo."
      (interactive)
      (let ((current-prefix-arg '(t)))
        (call-interactively #'magit-status)))))

(elpaca-setup magit-todos
  (:load-after magit)
  (:when-loaded
    (magit-todos-mode)))

;; -----------------------------------------
;; Project

;; Project manager.

;; Adding to project.el project directory detection
;; https://michael.stapelberg.ch/posts/2021-04-02-emacs-project-override/

(elpaca nil (setup project ; built-in
       (setq project-list-file (expand-file-name "projects" dot-cache-dir))
       (:when-loaded

         (defun dot/project-find (dir)
           (let ((root (locate-dominating-file dir ".project")))
             (and root (list 'vc 'Filewise root))))
         (add-hook 'project-find-functions #'dot/project-find)

         (defun dot/find-project-root ()
           "Return root of the project, determined by `.git/' and `.project',
`default-directory' otherwise."
           (let ((project (project-current)))
             (if project
                 (project-root project)
               default-directory)))

         (defun dot/find-file-in-project-root ()
           "Find file in project root."
           (interactive)
           (let ((default-directory (dot/find-project-root)))
             (call-interactively 'find-file)))

         (defun dot/project-remember-projects-under (dir maxdepth)
           "Index all projects below directory DIR recursively, until MAXDEPTH."
           (let ((files (mapcar 'file-name-directory
                                (dot/directory-files-recursively-depth
                                 dir "\\.git$\\|\\.project$" t maxdepth))))
             (dolist (path files)
               (project-remember-projects-under path))))

         (unless (file-exists-p project-list-file)
           (project-remember-projects-under "~/dotfiles")
           (dot/project-remember-projects-under "~/code" 4))

         (defun dot/project-project-name ()
           "Return project name."
           (let ((project (project-current)))
             (if project
                 (file-name-nondirectory (directory-file-name (project-root project)))
               "-")))

         (defun dot/project-save-project-buffers ()
           "Save all project buffers."
           (interactive)
           (let ((buffers (cl-remove-if (lambda (buffer) (not (buffer-file-name buffer)))
                                        (project-buffers (project-current)))))
             (save-some-buffers t (lambda () (member (current-buffer) buffers))))))))

;; -----------------------------------------
;; Compile

;; Enable color escape codes.

(elpaca nil (setup ansi-color ; built-in
       (:with-mode compilation-filter (:hook ansi-color-compilation-filter))
       ;; :hook (compilation-filter . ansi-color-compilation-filter)
       (:when-loaded (setq ansi-color-bold-is-bright t))))

(elpaca nil (setup compile ; built-in
       (defun dot/compile-disable-underline () ""
              (face-remap-add-relative 'underline :underline nil))
       (:with-mode comint-mode (:hook dot/compile-disable-underline))
       (:when-loaded
         (setq compilation-scroll-output 'first-error)

         (defun dot/compile ()
           "Compile project."
           (interactive)
           (let ((default-directory (dot/find-project-root)))
             (dot/project-save-project-buffers)
             (let ((current-prefix-arg '(t)))
               (call-interactively 'compile)))))))

;; -----------------------------------------
;; Languages

;;; Language Server Protocol

(elpaca-setup lsp-mode
  (:autoload lsp-deferred)
  ;; (:require lsp-modeline lsp-mode)
  (:with-mode
      (c-mode         ; clangd
       c++-mode       ; clangd
       php-mode       ; nodejs-intelephense
       csharp-mode    ; omnisharp-roslyn-bin
       lua-mode       ; lua-language-server
       latex-mode     ; texlab
       kotlin-mode    ; kotlin-language-server
       web-mode)
    (:hook lsp-deferred))
  (:when-loaded
    (setq lsp-auto-guess-root t)
    (setq lsp-clients-clangd-args '("-j=2"
                                    "--background-index"
                                    "--clang-tidy"
                                    "--compile-commands-dir=build"
                                    "--log=error"
                                    "--header-insertion-decorators=0"
                                    "--pch-storage=memory"
                                    "--enable-config"))
    (setq lsp-csharp-omnisharp-roslyn-binary-path "/usr/bin/omnisharp")
    (setq lsp-csharp-server-install-dir "/usr/lib/omnisharp/")
    (setq lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server")
    (setq lsp-clients-lua-language-server-install-dir "/usr/lib/lua-language-server/")
    (setq lsp-clients-lua-language-server-main-location "/usr/lib/lua-language-server/main.lua")
    (setq lsp-enable-xref t)
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-intelephense-storage-path (expand-file-name "lsp-cache" dot-cache-dir))
    (setq lsp-keep-workspace-alive nil)
    ;; (setq lsp-modeline-code-actions-enable nil)
    ;; (setq lsp-modeline-diagnostics-enable nil)
    ;; (setq lsp-modeline-workspace-status-enable nil)
    (setq lsp-prefer-flymake nil)
    (setq lsp-session-file (expand-file-name "lsp-session-v1" dot-cache-dir))

    ;; Mark clangd args variable as safe to modify via .dir-locals.el
    (put 'lsp-clients-clangd-args 'safe-local-variable #'listp)

    ;; Enable which-key descriptions
    (dolist (leader-key (list dot/leader-key dot/leader-alt-key))
      (let ((lsp-keymap-prefix (concat leader-key " l")))
        (lsp-enable-which-key-integration)))

    (defun dot/lsp-format-buffer-or-region ()
      "Format the selection (or buffer) with LSP."
      (interactive)
      (unless (bound-and-true-p lsp-mode)
        (message "Not in an LSP buffer"))
      (call-interactively
       (if (use-region-p)
           #'lsp-format-region
         #'lsp-format-buffer)))

    ;; This is cached to prevent unneeded I/O
    (setq lsp-in-cpp-project-cache nil)
    (defun dot/lsp-format-cpp-buffer ()
      "Format buffer in C++ projects."
      (unless lsp-in-cpp-project-cache
        (set (make-local-variable 'lsp-in-cpp-project-cache)
             (list
              (if (and (eq major-mode 'c++-mode)
                       (bound-and-true-p lsp-mode)
                       (or
                        (locate-dominating-file "." ".clang-format")
                        (locate-dominating-file "." "_clang-format")))
                  t
                nil))))
      (when (car lsp-in-cpp-project-cache)
        (lsp-format-buffer)))
    (add-hook 'before-save-hook #'dot/lsp-format-cpp-buffer)))

(elpaca-setup lsp-ui
  (:autoload lsp-ui-mode)
  (:load-after flycheck lsp-mode)
  (:when-loaded
    (setq lsp-ui-doc-border (face-foreground 'default))
    (setq lsp-ui-doc-delay 0.5)
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-header t)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-position 'top)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-flycheck-list-position 'right)
    (setq lsp-ui-imenu-enable nil)
    (setq lsp-ui-peek-enable nil)
    (setq lsp-ui-sideline-enable nil)))

;;; Debug Adapter Protocol

(elpaca-setup treemacs
  (:hook dot/hook-disable-line-numbers)
  (:when-loaded (setq treemacs-persist-file (expand-file-name "treemacs/persist" dot-cache-dir))))

(elpaca-setup lsp-treemacs
  (:load-after treemacs)
  (:when-loaded (setq lsp-treemacs-error-list-current-project-only t)))

(elpaca-setup dap-mode
  (:with-mode lsp-after-initialize (:hook dot/dap-install-debug-adapters))
  (:load-after lsp-treemacs lsp-mode)
  (:when-loaded
    (setq dap-auto-configure-features '(sessions locals expressions controls tooltip))
    (setq dap-breakpoints-file (expand-file-name "dap/breakpoints" dot-cache-dir))
    (setq dap-utils-extension-path (expand-file-name "dap" dot-cache-dir))

    ;; Create dap extension directory
    (unless (file-directory-p dap-utils-extension-path)
      (make-directory dap-utils-extension-path t))

    (defun dot/dap-install-debug-adapters ()
      "Install and Load debug adapters."
      (interactive)
      (unless (bound-and-true-p lsp-mode)
        (user-error "Not in an LSP buffer"))
      (when (string-equal major-mode "c++-mode")
        (require 'dap-cpptools)
        (dap-cpptools-setup)))))

;;; C/C++

(elpaca nil (setup c-mode ; built-in
       ;; C++ // line comment style in c-mode
       (defun dot/c-mode-comment-style () ""
              (c-toggle-comment-style -1))
       (:hook dot/c-mode-comment-style)))

;;; C#

;; Packages:
;; - dotnet-host
;; - dotnet-runtime-6.0
;; - dotnet-sdk-6.0
;; - dotnet-targeting-pack-6.0
;; - omnisharp-roslyn-bin
;; - netcoredbg (edit PKGBUILD to detect dotnet -6.0 dependencies)

(elpaca nil (setup csharp-mode)) ; built-in

;;; CMake

(elpaca-setup cmake-mode
  (:defer 2)
  (:when-loaded (setq cmake-tab-width (default-value 'tab-width))))

;;; Emacs Lisp

(elpaca nil (setup emacs-lisp ; built-in
       (defun dot/elisp-init () ""
              (setq-local indent-tabs-mode nil))
       (:hook dot/elisp-init)))

;;; GLSL

(elpaca-setup glsl-mode)

;;; HTML

(elpaca-setup web-mode)

;;; Kotlin

(elpaca-setup kotlin-mode)

;;; Lua

(elpaca-setup lua-mode
  (:when-loaded (setq lua-indent-level (default-value 'tab-width))))

;;; PHP

(elpaca-setup php-mode
  (defun dot/php-mode-init () ""
         (setq-local indent-tabs-mode t))
  (:hook dot/php-mode-init))

(elpaca-setup restclient)

;;; Python

(elpaca nil (setup python-mode ; built-in
       (defun dot/python-mode-init () ""
              (setq-local indent-tabs-mode t)
              (setq-local tab-width (default-value 'tab-width))
              (setq python-indent-offset (default-value 'tab-width)))
       (:hook dot/python-mode-init)))

;;; YAML

(elpaca-setup yaml-mode)
;; :defer t)

;; -----------------------------------------
;; Syntax Highlighting

;; (elpaca-setup tree-sitter-langs))

;; (elpaca-setup tree-sitter)
;;   (:also-load tree-sitter-langs)
;;   (:when-loaded
;;     (global-tree-sitter-mode)
;;     (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

;; -----------------------------------------
;; Quality of Life

;;; Flycheck, on the fly syntax checking

(elpaca-setup flycheck
  (:autoload flycheck-mode)
  (:with-mode
      (c-mode-common
       emacs-lisp-mode
       latex-mode
       org-mode
       php-mode
       sh-mode
       shell-mode
       shell-script-mode)
    (:hook flycheck-mode))
  (:when-loaded
    (setq flycheck-clang-language-standard "c++20")
    (setq flycheck-gcc-language-standard "c++20")))

;; For .el files which are intended to be packages
(elpaca-setup flycheck-package
  (:load-after flycheck)
  (:when-loaded
    (add-to-list 'flycheck-checkers 'flycheck-emacs-lisp-package)
    (flycheck-package-setup)))

(elpaca-setup flycheck-clang-tidy
  (:load-after flycheck)
  (:with-mode flycheck-mode (:hook flycheck-clang-tidy-setup))
  (:when-loaded (setq flycheck-clang-tidy-extra-options "--format-style=file")))

;;; Flyspell

;; Give Flyspell a selection menu.
(elpaca-setup flyspell-correct
  (:load-after flyspell)
  (:with-mode org-mode (:hook flyspell-mode))
  (:when-loaded
    (setq flyspell-issue-message-flag nil)
    (setq flyspell-issue-welcome-flag nil))

  (defun dot/flyspell-toggle ()
    "Toggle Flyspell, prompt for language."
    (interactive)
    (if (symbol-value flyspell-mode)
        (flyspell-mode -1)
      (call-interactively 'ispell-change-dictionary)
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode))
      (flyspell-buffer))))

;;; Rainbow Delimiters

(elpaca-setup rainbow-delimiters
  (:hook-into prog-mode))

;;; Rainbow Mode

(elpaca-setup rainbow-mode
  (:hook-into css-mode))

;;; YASnippet

(elpaca-setup yasnippet
  (:autoload yas-insert-snippet)
  (setq yas-snippet-dirs (list (expand-file-name "snippets" dot-emacs-dir)))
  (setq yas-prompt-functions '(yas-completing-prompt))
  (:when-loaded
    (yas-global-mode)))

(elpaca-setup yasnippet-snippets
  (:load-after yasnippet))

;; https://stackoverflow.com/questions/22735895/configuring-a-yasnippet-for-two-scenarios-1-region-is-active-2-region-is

(provide 'dot-development)

;;; dot-org-mode.el ends here
