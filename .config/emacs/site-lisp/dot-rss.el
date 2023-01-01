;;; dot-rss.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; RSS.

;;; Code:

;; -----------------------------------------

(elpaca-setup elfeed
  (:autoload elfeed)
  (:with-mode dot/hook-disable-line-numbers
    (:hook-into elfeed-search-mode)
    (:hook-into elfeed-show-mode))
  (:when-loaded
    (setq elfeed-db-directory (expand-file-name "elfeed" dot-cache-dir))
    (setq elfeed-enclosure-default-dir "~/downloads/")
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-search-clipboard-type 'CLIPBOARD)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-title-min-width 30)
    (setq elfeed-search-trailing-width 55)
    (setq elfeed-show-unique-buffers t)
    (load (expand-file-name "elfeed-feeds" dot-etc-dir))))

(provide 'dot-rss)

;;; dot-rss.el ends here
