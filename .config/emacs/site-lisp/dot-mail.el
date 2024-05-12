;;; dot-mail.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Mail configuration.

;;; Code:

;; -----------------------------------------
;; Mail Functions

(with-eval-after-load 'auth-source
  (defun dot/mail-auth-get-field (host prop)
    "Find PROP in `auth-sources' for HOST entry."
    (when-let ((source (auth-source-search :max 1 :host host)))
      (if (eq prop :secret)
          (funcall (plist-get (car source) prop))
        (plist-get (flatten-list source) prop)))))

;; Mail in Emacs with mu4e

;; Useful mu4e manual pages:

;; Key bindings
;; [https://www.djcbsoftware.nl/code/mu/mu4e/MSGV-Keybindings.html#MSGV-Keybindings]

(elpaca-nil (setup mu4e ; loaded from AUR package
       (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
       (:autoload mu4e mu4e-update-index)
       (:when-loaded
         (add-to-list 'auth-sources (expand-file-name "authinfo.gpg" dot-etc-dir))
         (setq user-full-name (dot/mail-auth-get-field "fullname" :user))
         (setq user-mail-address (dot/mail-auth-get-field "info" :user))
         (setq mail-user-agent 'mu4e-user-agent)

         ;; Headers
         (setq mu4e-headers-date-format "%d-%m-%Y")
         (setq mu4e-headers-time-format "%I:%M %p")
         (setq mu4e-headers-long-date-format "%d-%m-%Y %I:%M:%S %p")

         ;; Syncing
         (setq mu4e-get-mail-command (concat "mbsync -a -c " (expand-file-name "isync/mbsyncrc" (getenv "XDG_CONFIG_HOME"))))
         (setq mu4e-update-interval (* 15 60)) ; 15 minutes
         (setq mu4e-maildir "~/mail")
         (setq mu4e-attachment-dir "~/downloads")

         ;; Avoid mail syncing issues when using mbsync
         (setq mu4e-change-filenames-when-moving t)

         ;; Misc
         (setq mu4e-completing-read-function 'completing-read)
         (setq mu4e-confirm-quit nil)
         (setq mu4e-display-update-status-in-modeline t)
         (setq mu4e-hide-index-messages t)
         (setq mu4e-sent-messages-behavior 'sent)
         (setq mu4e-view-show-addresses t)
         (setq mu4e-view-show-images nil)

         ;; Compose
         (setq mu4e-compose-context-policy 'ask)
         (setq mu4e-compose-dont-reply-to-self t)
         (setq mu4e-compose-signature (concat (dot/mail-auth-get-field "fullname" :user) "\nriyyi.com\n"))
         (setq mu4e-compose-signature-auto-include t)

         ;; Contexts
         (setq mu4e-context-policy 'pick-first)
         (setq mu4e-contexts
               `(,(make-mu4e-context
                   :name "info"
                   :match-func (lambda (msg)
                                 (when msg
                                   (string= (mu4e-message-field msg :maildir) "/info")))
                   :vars `((user-mail-address  . ,(dot/mail-auth-get-field "info" :user))
                           (mu4e-drafts-folder . "/info/Drafts")
                           (mu4e-refile-folder . "/info/Archive")
                           (mu4e-sent-folder   . "/info/Sent")
                           (mu4e-trash-folder  . "/info/Trash")))
                 ,(make-mu4e-context
                   :name "private"
                   :match-func (lambda (msg)
                                 (when msg
                                   (string= (mu4e-message-field msg :maildir) "/private")))
                   :vars `((user-mail-address  . ,(dot/mail-auth-get-field "private" :user))
                           (mu4e-drafts-folder . "/private/Drafts")
                           (mu4e-refile-folder . "/private/Archive")
                           (mu4e-sent-folder   . "/private/Sent")
                           (mu4e-trash-folder  . "/private/Trash")))
                 ))

         ;; Do not mark messages as IMAP-deleted, just move them to the Trash directory!
         ;; https://github.com/djcb/mu/issues/1136#issuecomment-486177435
         (setf (alist-get 'trash mu4e-marks)
               (list :char '("d" . "▼")
                     :prompt "dtrash"
                     :dyn-target (lambda (target msg)
                                   (mu4e-get-trash-folder msg))
                     :action (lambda (docid msg target)
                               (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))

         ;; Start mu4e in the background for mail syncing
         (mu4e t))))

;; Use mu4e-alert to show new e-mail notifications.
;; https://github.com/iqbalansari/mu4e-alert

(elpaca-setup mu4e-alert
  (:defer 20)
  (:when-loaded
    (setq mu4e-alert-interesting-mail-query "(maildir:/info/Inbox OR maildir:/private/Inbox) AND flag:unread AND NOT flag:trashed")
    (setq mu4e-alert-notify-repeated-mails nil)

    (mu4e-alert-set-default-style 'libnotify)
    (mu4e-alert-enable-notifications)))

;; Sending mail.

(elpaca-nil (setup smtpmail ; built-in
       (setq smtpmail-default-smtp-server "mail.riyyi.com")
       (:load-after mu4e)
       (:when-loaded
         (setq smtpmail-smtp-server "mail.riyyi.com")
         (setq smtpmail-local-domain "riyyi.com")
         (setq smtpmail-smtp-service 587)
         (setq smtpmail-stream-type 'starttls)
         (setq smtpmail-queue-mail nil))))

(elpaca-nil (setup sendmail ; built-in
       (:load-after mu4e)
       (:when-loaded (setq send-mail-function 'smtpmail-send-it))))

(elpaca-nil (setup message ; built-in
       (:load-after mu4e)
       (:when-loaded
         (setq message-kill-buffer-on-exit t)
         (setq message-send-mail-function 'smtpmail-send-it))))

;; Sources:
;; - https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
;; - https://wiki.archlinux.org/title/Isync
;; - https://man.archlinux.org/man/community/isync/mbsync.1.en
;; - https://gitlab.com/protesilaos/dotfiles/-/blob/master/mbsync/.mbsyncrc
;; - https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-mail.el
;; - https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-mu4e-deprecated-conf.el
;; - https://github.com/daviwil/dotfiles/blob/master/Mail.org

(provide 'dot-mail)

;;; dot-mail.el ends here
