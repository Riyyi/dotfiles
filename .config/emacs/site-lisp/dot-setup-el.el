;;; dot-setup.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure SetupEl and keywords.

;;; Code:

;; -----------------------------------------
;; SetupEl

;; Install and load SetupEl immediately
(elpaca setup (require 'setup)

;;; Configure custom macros

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :defer
  (lambda (secs)
    `(if (numberp ,secs)
         (run-with-idle-timer ,secs nil (lambda () (require ',(setup-get 'feature))))
       ,(setup-quit)))
  :documentation "Defer loading of feature for SECS idle seconds.")

;;; Configure custom macros for evil bindings

(setup-define :with-evil-state
  (lambda (states &rest body)
    (setup-bind body (states states)))
  :documentation "Change the STATE that BODY will bind to."
  :indent 1)

(setup-define :evil-bind-impl
  (lambda (key command)
    `(evil-define-key* ',(setup-get 'states) ,(setup-get 'map) ,key ,command))
  :documentation "Bind KEY to COMMAND in the current map."
  :after-loaded t
  :ensure '(kbd func)
  :repeatable t)

(setup-define :evil-bind
  (lambda (states &rest keybinds)
    `(with-eval-after-load 'evil
       (:with-evil-state ,states (:evil-bind-impl ,@keybinds))))
  :documentation "Bind KEYBINDS in STATES in the current map.")

(setup-define :evil-bind-into
  (lambda (states feature-or-map &rest keybinds)
    (if (string-match-p "-map\\'" (symbol-name feature-or-map))
        `(:with-map ,feature-or-map (:evil-bind ,states ,@keybinds))
      `(:with-feature ,feature-or-map (:evil-bind ,states ,@keybinds))))
  :documentation "Bind KEYBINDS in STATE into the map of FEATURE-OR-MAP.
The arguments REST are handled as by `:evil-bind'."
  :indent 1)
)

;;; Add setup.el macro

;;;###autoload
(defmacro elpaca-setup (order &rest body)
  "Execute BODY in `setup' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by elpaca-setup" order)
    (let ((o order))
      (when-let ((ensure (cl-position :ensure body)))
        (setq o (if (null (nth (1+ ensure) body)) nil order)
              body (append (cl-subseq body 0 ensure)
                           (cl-subseq body (+ ensure 2)))))
      `(elpaca ,o (setup
                    ,(if-let (((memq (car-safe order) '(quote \`)))
                              (feature (flatten-tree order)))
                         (cadr feature)
                       (elpaca--first order))
                    ,@body)))))

;; Startup benchmark
(elpaca-setup benchmark-init
  (:require benchmark-init)
  (:when-loaded
    (benchmark-init/activate)
    (add-hook 'elpaca-after-init-hook #'benchmark-init/deactivate)))

(provide 'dot-setup-el)

;;; dot-setup.el ends here
