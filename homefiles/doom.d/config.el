;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Use , as shortcut for SPC-m
(setq doom-localleader-key ",")

;; Popup repl to the right in vsplit
(after! cider
  (set-popup-rule! "^\\*cider-repl" :side 'right :quit nil :size 0.5))

;; Better configuration for lisp
(after! lispy
  (lispy-set-key-theme '(lispy c-digits paredit))
  (define-key lispy-mode-map (kbd "[") 'lispy-open-square)
  (define-key lispy-mode-map (kbd "]") 'lispy-close-square))

(setq doom-font (font-spec :family "Menlo" :size 20))
