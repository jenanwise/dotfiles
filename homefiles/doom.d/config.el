;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Use , as shortcut for SPC-m
(setq doom-localleader-key ",")

(after! cider
  ;; Popup repl to the right in vsplit
  (set-popup-rule! "^\\*cider-repl" :side 'right :quit nil :size 0.5)

  ;; fipp is much faster than the default printer
  (setq cider-print-fn 'fipp))

;; Better configuration for lisps
(after! lispy
  (lispy-set-key-theme '(lispy c-digits paredit))

  ;; The default [ and ] are navigation, which makes
  ;; clojure difficult to type.
  (define-key lispy-mode-map (kbd "[") 'lispy-open-square)
  (define-key lispy-mode-map (kbd "]") 'lispy-close-square))
  (define-key lispy-mode-map (kbd "}") 'lispy-close-curly)

;; Bigger font for tired eyes
(setq doom-font (font-spec :family "Menlo" :size 20))

;; Turn on clj-kondo linting because it's great
(require 'flycheck-clj-kondo)
