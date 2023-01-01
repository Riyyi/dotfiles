;;; dot-keybinds.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;  All keybinds.

;;; Code:

;; -----------------------------------------
;; Useful links

;; Mastering Emacs key bindings
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;; use-package bind key
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el

;; GNU remapping commands
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Remapping-Commands.html

;; GNU binding combinations of modifiers
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Binding-combinations-of-modifiers-and-function-keys.html

;; Doom Emacs bindings
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el

;; Keybindings and States
;; https://github.com/noctuid/evil-guide#keybindings-and-states

;; General.el
;; https://github.com/noctuid/general.el

;; -----------------------------------------
;; Disable Native

;; Disable keybinds of native modes that clash with other custom keybinds.

(elpaca nil (setup emacs
       (:global
        "M-h" nil
        "M-j" nil
        "M-k" nil
        "M-l" nil
        )))

(elpaca nil (setup org
       (:bind "M-h" nil
              "C-M-h" nil
              )))

(elpaca nil (setup cc-mode
       (:bind-into c-mode-base-map
         "M-j" nil
         "C-M-h" nil
         )))

(elpaca nil (setup nxml-mode
       (:bind "M-h" nil
              )))

;; -----------------------------------------
;; Disable Package

;; Disable keybinds of installed packages that clash with other custom keybinds.

(elpaca nil (setup evil-states
       (:bind-into evil-motion-state-map dot/leader-key nil
                   )))

(elpaca nil (setup magit
       (:evil-bind normal
                   ;; Do not close magit when pressing escape
                   "<escape>" nil)))

(elpaca nil (setup php-mode
       (:bind "M-j" nil
              "C-M-h" nil
              )))

;; -----------------------------------------
;; Set Native

;; Set keybinds to native functionality.

;;; Set Native Global Keybinds

(elpaca nil (setup emacs
       (:global
        ;; Buffers
        "C-x C-b" ibuffer
        "M-w" kill-buffer-and-window

        ;; Config edit/reload
        "C-c r" dot/config-reload
        "C-c v" dot/config-visit

        ;; Find file
        "C-x C-f" dot/find-file-in-project-root

        ;; Split and follow window
        "C-x 2" split-follow-horizontally
        "C-x 3" split-follow-vertically

        ;; Terminal
        "<s-backspace>" ansi-term

        ;; Universal prefix argument
        "C-M-u" universal-argument
        )))

;;; Set Native Mode Keybinds

;; Dired
(elpaca nil (setup dired
       (:bind
        [remap dired-find-file] dot/dired-find-file
        [remap dired-up-directory] dot/dired-up-directory
        )))

;; Org
(elpaca nil (setup org
       (:bind "M-c" org-edit-special
              )))
(elpaca nil (setup org-src
       (:bind "M-c" org-edit-src-exit
              "M-k" org-edit-src-abort
              )))
(elpaca nil (setup org-capture
       (:bind "M-c" org-capture-finalize
              "M-w" org-capture-refile
              "M-k" org-capture-kill
              )))

;; -----------------------------------------
;; Set Package

;; Set keybinds to functionality of installed packages.

(elpaca nil (setup emacs
       (:global
        ;; Buffers
        "M-h" centaur-tabs-backward-tab
        "M-j" centaur-tabs-forward-group
        "M-k" centaur-tabs-backward-group
        "M-l" centaur-tabs-forward-tab
        "M-H" centaur-tabs-move-current-tab-to-left
        "M-L" centaur-tabs-move-current-tab-to-right
        "M-\`" evil-switch-to-windows-last-buffer
        ;; Other
        "M-s" avy-goto-char-timer
        "M-x" dot/M-x
        )))

(elpaca nil (setup company
       (:bind-into company-active-map
         ;; Company completion selection
         "M-n" nil
         "M-p" nil
         "M-h" company-abort
         "M-j" company-select-next
         "M-k" company-select-previous
         "M-l" company-complete-selection
         "<escape>" company-abort
         )))

(elpaca nil (setup evil-ex
       (:bind-into evil-ex-completion-map
         ;; Evil command history selection
         "M-h" abort-recursive-edit
         "M-j" next-complete-history-element
         "M-k" previous-complete-history-element
         "M-l" exit-minibuffer
         )))

(elpaca nil (setup emacs
       (:global
        ;; flyspell-correct
        [remap ispell-word] flyspell-correct-at-point ; z=

        ;; Helpful overwrite default help functions
        [remap describe-command] helpful-command
        [remap describe-function] helpful-callable
        [remap describe-key] helpful-key
        [remap describe-symbol] helpful-at-point
        [remap describe-variable] helpful-variable
        )
       (setup which-key
         (:when-loaded
           (which-key-add-key-based-replacements "C-h o" "describe-symbol-at-point")))))

;; LSP
(elpaca nil (setup lsp-mode
       (:bind-into lsp-signature-mode-map
         "M-j" lsp-signature-next
         "M-k" lsp-signature-previous
         )))

;; Magit
(elpaca nil (setup magit
       (:bind-into magit-log-select-mode-map
         "M-c" magit-log-select-pick
         "M-k" magit-log-select-quit
         )))

;; Org-roam
(elpaca nil (setup org-roam
       (:bind [down-mouse-1] org-roam-visit-thing
              )))

;; Minibuffer completion selection
(elpaca nil (setup minibuffer
       (:bind-into minibuffer-local-map
         "M-J" next-history-element
         "M-K" previous-history-element
         "M-h" abort-recursive-edit
		 "M-i" vertico-quick-insert
         "M-j" vertico-next
         "M-k" vertico-previous
         "M-l" vertico-exit
		 "M-m" vertico-quick-exit
         "<backspace>"   dot/vertico-backspace
         "<S-backspace>" evil-delete-backward-char-and-join
         )))

;; with-editor
(elpaca nil (setup with-editor
       (:bind
        "M-c" with-editor-finish
        "M-k" with-editor-cancel
        )))

;;; Global evil keymap

(elpaca nil (setup evil
       (:bind-into evil-normal-state-map
         "C-n"      neotree-toggle-in-project-root
         "C-S-p"    evil-paste-pop-next
         "S-<up>"   scroll-down-line
         "S-<down>" scroll-up-line
         )

       (:bind-into evil-insert-state-map
         "<backtab>" dot/evil-insert-shift-left  ; <S-Tab>
         "TAB"       dot/evil-insert-shift-right ; <Tab>
         )

       (:bind-into evil-visual-state-map
         "<" dot/evil-visual-shift-left   ; <gv
         ">" dot/evil-visual-shift-right  ; >gv
         )

       (:bind-into evil-ex-map
         "e" dot/find-file-in-project-root
         )))

;;; Other evil state-related keybinds

;; Custom (M-x customize)
(elpaca nil (setup cus-edit
       (:evil-bind-into normal custom-mode-map
                        [down-mouse-1] widget-button-click
                        )))

;; Dashboard
(elpaca nil (setup dashboard
       (:evil-bind normal
                   [down-mouse-1] widget-button-click
                   "g" dashboard-refresh-buffer
                   "m" dot/dashboard-goto-bookmarks
                   "p" dot/dashboard-goto-projects
                   "r" dot/dashboard-goto-recent-files
                   )))

;; Dap
(elpaca nil (setup dap-ui
       (:evil-bind-into normal dap-ui-session-mode-map
                        "D" dap-ui-delete-session
                        )))

;; Deft
(elpaca nil (setup deft
       (:evil-bind normal
                   [down-mouse-1] widget-button-click
                   "+" deft-new-file-named
                   "-" deft-new-file
                   "a" deft-archive-file
                   "c" deft-filter-clear
                   "d" deft-delete-file
                   "f" deft-find-file
                   "g" deft-refresh
                   "q" kill-this-buffer
                   "R" deft-rename-file
                   "s" deft-filter
                   "ts" '("Toggle search" . deft-toggle-incremental-search) ; which-key
                   "tt" '("Toggle sort" . deft-toggle-sort-method)          ; custom string
                   )))

;; Elfeed
(elpaca nil (setup elfeed
       (:evil-bind-into normal elfeed-search-mode-map
                        "b"  elfeed-search-browse-url
                        "c"  elfeed-search-clear-filter
                        "gr" '("Refresh buffer" . elfeed-search-update--force)
                        "gR" '("Update feeds" . elfeed-search-fetch)
                        "q"  elfeed-search-quit-window
                        "u"  elfeed-search-tag-all-unread
                        "U"  nil
                        "r"  elfeed-search-untag-all-unread
                        )
       (:evil-bind-into normal elfeed-show-mode-map
                        "b" elfeed-show-visit
                        "g" elfeed-show-refresh
                        "q" elfeed-kill-buffer
                        "u" elfeed-show-tag--unread
                        "y" elfeed-show-yank
                        )))

;; Magit
(elpaca nil (setup magit
       (:evil-bind (normal visual)
                   "{" magit-section-backward-sibling
                   "}" magit-section-forward-sibling
                   )))

;; Minibuffer
(elpaca nil (setup minibuffer
       (:evil-bind-into normal minibuffer-local-map
                        "TAB"    vertico-insert
                        "j"      vertico-next
                        "k"      vertico-previous
                        "<up>"   vertico-previous
                        "<down>" vertico-next
                        )
       (:evil-bind-into insert minibuffer-local-map
                        "TAB"    vertico-insert
                        )))

;; Mu4e
(elpaca nil (setup mu4e
       (:evil-bind-into normal mu4e-compose-mode-map
                        "q"   mu4e-message-kill-buffer
                        "M-c" message-send-and-exit
                        "M-k" mu4e-message-kill-buffer
                        )))

;; Neotree
(elpaca nil (setup neotree
       (:evil-bind normal
                   "RET"       neotree-enter
                   "<backtab>" neotree-collapse-all ; <S-tab>
                   "c"         neotree-create-node
                   "r"         neotree-rename-node
                   "d"         neotree-delete-node
                   "h"         neotree-select-previous-sibling-node
                   "g"         neotree-refresh
                   "j"         neotree-next-line
                   "k"         neotree-previous-line
                   "l"         neotree-enter
                   "C"         neotree-change-root
                   "H"         neotree-hidden-file-toggle
                   "q"         neotree-hide
                   )))

;; Org
(elpaca nil (setup org
       (:evil-bind normal
                   "RET" dot/org-ret-at-point
                   )
       (:evil-bind insert
                   "RET" evil-ret
                   )
       (:evil-bind-into motion org-agenda-mode-map
                        "RET" org-agenda-switch-to
                        )))

;; Wdired
(elpaca nil (setup wdired
       (:evil-bind (normal insert)
                   "M-c" wdired-finish-edit
                   "M-k" wdired-abort-changes
                   )))

;; -----------------------------------------
;; Set leader key

;; General.el ~leader key binds.

;;; Global Leader

(elpaca nil (setup general
       (:when-loaded
         (general-create-definer space-leader
           :prefix dot/leader-key
           :non-normal-prefix dot/leader-alt-key
           :global-prefix dot/leader-alt-key
           :states '(normal visual insert motion emacs)
           :keymaps 'override) ; prevent leader keybindings from ever being overridden

         (space-leader
           "SPC"       '(dot/M-x                    :which-key "Execute command")
           "RET"       '(consult-bookmark           :which-key "Go to bookmark")

           ;; Apps
           "a"     '(:ignore t                      :which-key "apps")
           "a d"   '(deft                           :which-key "Deft")
           "a e"   '(elfeed                         :which-key "Elfeed")

           ;; Buffer / bookmark
           "b"         '(:ignore t                         :which-key "buffer/bookmark")
           "b a"       '(auto-revert-mode                  :which-key "Auto revert buffer")
           "b b"       '(consult-buffer                    :which-key "Switch buffer")
           "b d"       '(dot/dashboard-goto                :which-key "Dashboard")
           "b k"       '(kill-current-buffer               :which-key "Kill buffer")
           "b m"       '(bookmark-set                      :which-key "Make bookmark")
           "b n"       '(evil-buffer-new                   :which-key "New empty buffer")
           "b r"       '(revert-buffer                     :which-key "Revert buffer")
           "b s"       '(basic-save-buffer                 :which-key "Save buffer")
           "b B"       '(ibuffer                           :which-key "List buffers")
           "b C"       '(dot/centaur-tabs-buffer-cleanup   :which-key "Cleanup buffers")
           "b M"       '(bookmark-delete                   :which-key "Delete bookmark")
           "b S"       '(evil-write-all                    :which-key "Save all buffers")
           "b <left>"  '(previous-buffer                   :which-key "Previous buffer")
           "b <right>" '(next-buffer                       :which-key "Next buffer")

           ;; Comments
           "c"   '(:ignore t                               :which-key "comment/config")
           "c c" '(evilnc-comment-or-uncomment-lines       :which-key "Toggle comment")
           "c p" '(evilnc-comment-or-uncomment-paragraphs  :which-key "Toggle comment paragraph")
           "c y" '(evilnc-comment-and-kill-ring-save       :which-key "Comment and copy")

           ;; Elisp
           "e"   '(:ignore t                        :which-key "elisp")
           "e ;" '(eval-expression                  :which-key "Evaluate expression")
           "e b" '(eval-buffer                      :which-key "Evaluate buffer")
           "e e" '(eval-last-sexp                   :which-key "Evaluate last sexp")
           "e r" '(eval-region                      :which-key "Evaluate region")
           "e t" '(dot/reload-theme                 :which-key "Reload theme")

           ;; File
           "f"     '(:ignore t                      :which-key "file")
           "f d"   '(dired                          :which-key "Find directory")
           "f f"   '(dot/find-file-in-project-root  :which-key "Find file")
           "f o"   '(ff-find-other-file             :which-key "Find header/source file")
           "f r"   '(consult-recent-file            :which-key "Find recent file")
           "f R"   '(rename-file-and-buffer         :which-key "Rename file")
           "f s"   '(basic-save-buffer              :which-key "Save file")
           "f S"   '(write-file                     :which-key "Save file as...")
           "f u"   '(dot/sudo-find-file             :which-key "Sudo find file")
           "f U"   '(dot/sudo-this-file             :which-key "Sudo this file")
           "f e"   '(:ignore t                      :which-key "emacs")
           "f e c" '(dot/config-visit               :which-key "Config visit")
           "f e f" '(dot/find-file-emacsd           :which-key "Find emacs file")
           "f e r" '(dot/config-reload              :which-key "Config reload")

           ;; Go to
           "g"     '(:ignore t                      :which-key "goto")
           "g b"   '(consult-bookmark               :which-key "Go to bookmark")
           "g f"   '(consult-flycheck               :which-key "Go to flycheck error")
           "g m"   '(consult-mark                   :which-key "Go to marker")

           ;; Help
           "h"   '(:keymap help-map                 :which-key "help")
           "h o" '(:ignore t                        :which-key "describe-symbol-at-point")

           ;; Insert
           "i"   '(:ignore t                        :which-key "insert")
           "i b" '(dot/indent-buffer                :which-key "Indent buffer")
           "i f" '(fill-region                      :which-key "Reflow region")
           "i F" '(fill-paragraph                   :which-key "Reflow paragraph")
           "i r" '(indent-region                    :which-key "Indent region")
           "i s" '(dot/evil-normal-sort-paragraph   :which-key "Sort paragraph")
           "i S" '(dot/insert-spaces-until-column   :which-key "Insert spaces")
           "i y" '(yas-insert-snippet               :which-key "Insert yasnippet")

           ;; Notes
           "n"     '(:ignore t                           :which-key "notes")
           "n a"   '(org-agenda                          :which-key "Org agenda")
           "n r"   '(:ignore t                           :which-key "org-roam")
           "n r c" '(org-roam-capture                    :which-key "Capture")
           "n r C" '(org-roam-db-sync                    :which-key "Build cache")
           "n r f" '(org-roam-node-find                  :which-key "Find node")
           "n r g" '(org-roam-graph                      :which-key "Show graph")
           "n r i" '(org-roam-node-insert                :which-key "Insert")
           "n r I" '(dot/org-roam-node-insert-immediate  :which-key "Insert (without capture)")
           "n r r" '(org-roam-buffer-toggle              :which-key "Toggle buffer")
           "n r s" '(org-roam-ui-mode                    :which-key "Toggle server")

           ;; Project
           "p"   '(:keymap project-prefix-map       :which-key "project")
           "p b" '(consult-project-buffer           :which-key "project-switch-buffer")
           "p f" '(consult-project-extra-find       :which-key "project-find-file")
           "p g" '(consult-grep                     :which-key "project-find-regexp")

           ;; Quit
           "q"   '(:ignore t                        :which-key "quit")
           "q q" '(save-buffers-kill-terminal       :which-key "Quit Emacs")
           "q Q" '(save-buffers-kill-emacs          :which-key "Quit Emacs (and daemon)")
           "q f" '(delete-frame                     :which-key "Close frame")
           "q o" '(delete-other-frames              :which-key "Close other frames")

           ;; Search
           "s"   '(:ignore t                        :which-key "search")
           "s a" '(avy-goto-char-timer              :which-key "Avy goto char")
           "s f" '(consult-find                     :which-key "Search file")
           "s l" '(avy-goto-line                    :which-key "Avy goto line")
           "s p" '(consult-grep                     :which-key "Search project")
           "s q" '(evil-ex-nohighlight              :which-key "Stop search")
           "s s" '(consult-line                     :which-key "Search buffer")

           ;; Tabs / toggle
           "t"   '(:ignore t                                 :which-key "tabs/toggle")
           "t f" '(dot/toggle-fringe                         :which-key "Toggle fringe")
           "t g" '(centaur-tabs-switch-group                 :which-key "Switch tab group")
           "t h" '(centaur-tabs-backward-group               :which-key "Tab backward group")
           "t j" '(centaur-tabs-select-end-tab               :which-key "Tab select first")
           "t k" '(centaur-tabs-select-beg-tab               :which-key "Tab select last")
           "t l" '(centaur-tabs-forward-group                :which-key "Tab forward group")
           "t n" '(neotree-toggle-in-project-root            :which-key "Toggle Neotree")
           "t s" '(dot/flyspell-toggle                       :which-key "Toggle spell checker")
           "t w" '(visual-line-mode                          :which-key "Toggle line wrapping")

           ;; Update packages
           "U"   '(elpaca-update-all                         :which-key "Update packages")

           ;; Version control
           "v"     '(:ignore t                      :which-key "git")
           "v b"   '(magit-branch-checkout          :which-key "Magit switch branch")
           "v B"   '(magit-blame-addition           :which-key "Magit blame")
           "v C"   '(magit-clone                    :which-key "Magit clone")
           "v F"   '(magit-fetch                    :which-key "Magit fetch")
           "v L"   '(magit-log                      :which-key "Magit log")
           "v s"   '(magit-show-commit              :which-key "Magit show commit")
           "v S"   '(magit-stage-file               :which-key "Stage file")
           "v U"   '(magit-unstage-file             :which-key "Unstage file")
           "v v"   '(magit-status                   :which-key "Magit status")
           "v V"   '(magit-status-here              :which-key "Magit status here")
           "v c"   '(:ignore t                      :which-key "create")
           "v c c" '(magit-commit-create            :which-key "Commit")
           "v c b" '(magit-branch-and-checkout      :which-key "Branch")
           "v c r" '(magit-init                     :which-key "Initialize repo")
           "v f"   '(:ignore t                      :which-key "file")
           "v f c" '(magit-find-git-config-file     :which-key "Find gitconfig file")
           "v f D" '(magit-file-delete              :which-key "Delete file")
           "v f f" '(magit-find-file                :which-key "Find file")
           "v f R" '(magit-file-rename              :which-key "Rename file")
           "v l"   '(:ignore t                      :which-key "list")
           "v l r" '(magit-list-repositories        :which-key "List repositories")
           "v l s" '(magit-list-submodules          :which-key "List submodules")
           "v r"   '(dot/magit-select-repo          :which-key "Select repo")

           ;; Window
           "w"         '(:ignore t                               :which-key "window")
           "w +"       '(evil-window-increase-height             :which-key "Increase window height")
           "w -"       '(evil-window-decrease-height             :which-key "Decrease window height")
           "w <"       '(evil-window-decrease-width              :which-key "Decrease window width")
           "w ="       '(balance-windows                         :which-key "Balance windows")
           "w >"       '(evil-window-increase-width              :which-key "Increase window width")
           "w _"       '(evil-window-set-height                  :which-key "Maximize window height")
           "w h"       '(windmove-left                           :which-key "Focus window left")
           "w j"       '(windmove-down                           :which-key "Focus window down")
           "w k"       '(windmove-up                             :which-key "Focus window up")
           "w l"       '(windmove-right                          :which-key "Focus window right")
           "w o"       '(delete-other-windows                    :which-key "Close other windows")
           "w s"       '(split-follow-horizontally               :which-key "Split horizontal")
           "w v"       '(split-follow-vertically                 :which-key "Split vertical")
           "w w"       '(other-window                            :which-key "Focus other window")
           "w q"       '(dot/centaur-tabs-kill-buffer-or-window  :which-key "Close window")
           "w r"       '(winner-redo                             :which-key "Redo window configuration")
           "w u"       '(winner-undo                             :which-key "Undo window configuration")
           "w <left>"  '(windmove-left                           :which-key "Focus window left")
           "w <right>" '(windmove-right                          :which-key "Focus window right")
           "w <up>"    '(windmove-up                             :which-key "Focus window up")
           "w <down>"  '(windmove-down                           :which-key "Focus window down")
           )

         ;; Evaluated keybinds.

         (with-eval-after-load 'lsp-mode
           (space-leader lsp-mode-map
             "l" lsp-command-map
             "l = f" '(dot/lsp-format-buffer-or-region :which-key "format buffer or region")
             ))

         (with-eval-after-load 'dap-mode
           (space-leader lsp-mode-map
             "l d" '(dap-hydra :which-key "DAP hydra")
             )))))

;; Source:
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-evil.el#L712
;; https://github.com/suyashbire1/emacs.d/blob/master/init.el

;;; Local Leader

(elpaca nil (setup general
       (:when-loaded
         (general-create-definer local-leader
           :prefix dot/localleader-key
           :non-normal-prefix dot/localleader-alt-key
           :global-prefix dot/localleader-alt-key
           :states '(normal visual insert motion emacs)
           :keymaps 'override ; prevent leader keybindings from ever being overridden
           ""    '(:ignore t                            :which-key "<localleader>")
           )

         (local-leader c++-mode-map
           "i"         '(:ignore t                             :which-key "insert")
           "i i"       '(dot/copy-cpp-function-implementation  :which-key "Copy function implementation")
           )

         (local-leader org-mode-map
           "'"         '(org-edit-special               :which-key "Org edit")
           "e"         '(org-export-dispatch            :which-key "Org export")
           "o"         '(org-open-at-point              :which-key "Org open at point")
           "q"         '(org-set-tags-command           :which-key "Org tags")

           "g"         '(:ignore t                      :which-key "goto")
           "g o"       '(consult-outline                :which-key "Org go to heading")

           "i"         '(:ignore t                      :which-key "insert")
           "i c"       '(org-table-insert-column        :which-key "Insert table column")
           "i h"       '(org-table-insert-hline         :which-key "Insert table hline")
           "i H"       '(org-table-hline-and-move       :which-key "Insert table hline and move")
           "i r"       '(org-table-insert-row           :which-key "Insert table row")
           "i t"       '(org-insert-structure-template  :which-key "Insert template")

           "l"         '(:ignore t                      :which-key "links")
           "l i"       '(org-id-store-link              :which-key "Store ID link")
           "l l"       '(org-insert-link                :which-key "Insert link")
           "l s"       '(org-store-link                 :which-key "Store link")
           "l S"       '(org-insert-last-stored-link    :which-key "Insert stored link")

           "s"         '(:ignore t                      :which-key "tree/subtree")
           "s h"       '(org-promote-subtree            :which-key "Org promote subtree")
           "s j"       '(org-metadown                   :which-key "Org move subtree down")
           "s k"       '(org-metaup                     :which-key "Org move subtree up")
           "s l"       '(org-demote-subtree             :which-key "Org demote subtree")
           "s <left>"  '(org-promote-subtree            :which-key "Org promote subtree")
           "s <right>" '(org-demote-subtree             :which-key "Org demote subtree")
           "s <up>"    '(org-move-subree-up             :which-key "Org move subtree up")
           "s <down>"  '(org-move-subtree-down          :which-key "Org move subtree down")

           "t"         '(:ignore t                      :which-key "toggle")
           "t t"       '(org-todo                       :which-key "Org todo state")
           "t l"       '(org-toggle-link-display        :which-key "Org link display")
           )

         (local-leader org-src-mode-map
           "k"         '(org-edit-src-abort             :which-key "Org Edit abort"))

         (local-leader elfeed-search-mode-map
           "g"         '(elfeed-search-update--force    :which-key "Elfeed refresh buffer")
           "G"         '(elfeed-search-fetch            :which-key "Elfeed update feeds")
           )

         (local-leader elfeed-show-mode-map
           "g"         '(elfeed-show-refresh            :which-key "Elfeed refresh buffer")
           ))))

;; c-fill-paragraph Reflow comment
;; https://youtu.be/hbmV1bnQ-i0?t=1910

(provide 'dot-keybinds)

;;; dot-keybinds.el ends here
