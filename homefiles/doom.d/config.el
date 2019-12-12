;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Use , as shortcut for SPC-m
(setq doom-localleader-key ",")

(after! cider
  ;; Popup cider panels to the right in vsplit
  (set-popup-rule! "^\\*cider-repl" :side 'right :quit nil :size 0.5 :modeline t)
  (set-popup-rule! "^\\*cider-scratch\\*" :side 'right :quit nil :size 0.5 :modeline t)

  ;; fipp is much faster than the default printer
  (setq cider-print-fn 'fipp))

(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;; Better configuration for lisps
(after! lispy
  (lispy-set-key-theme '(lispy c-digits paredit))

  ;; The default [ and ] are navigation, which makes
  ;; clojure difficult to type.
  (define-key lispy-mode-map (kbd "[") 'lispy-open-square)
  (define-key lispy-mode-map (kbd "]") 'lispy-close-square)
  (define-key lispy-mode-map (kbd "}") 'lispy-close-curly))

;; Bigger font for tired eyes
;(setq doom-font (font-spec :family "Menlo" :size 20))

;; Turn on clj-kondo linting because it's great
(require 'flycheck-clj-kondo)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (set-popup-rule! "^\\*rustic-compilation*" :side 'bottom :size 0.5 :select t)
  )
