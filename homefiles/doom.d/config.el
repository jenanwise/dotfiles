;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Use , as shortcut for SPC-m
(setq doom-localleader-key ",")

;; Popup repl to the right in vsplit
(after! clojure
  (set-popup-rule! "^\\*cider-repl"
    :quit nil
    :side 'right
    :size 0.5))

;; Better configuration for lisp.
(after! lispy
  (define-key lispy-mode-map (kbd "[") 'lispy-open-square)
  (define-key lispy-mode-map (kbd "]") 'lispy-close-square)
  (lispy-set-key-theme '(lispy c-digits paredit)))
