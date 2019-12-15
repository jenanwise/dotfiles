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

(defun rustic-cargo-run-current (&optional args-str)
  "Run the current buffer if it's a src/bin/*.rs file; else default."
  (interactive)
  (let ((buf (buffer-file-name)))
    (save-match-data
      (let* ((args-suffix (and args-str (concat " " args-str)))
             (suffix (if (string-match "src/bin/\\([a-zA-Z0-9_]+\\).rs" buf)
                         (concat " --bin=" (match-string 1 buf) args-suffix)
                       args-suffix))
             (cmd (concat "cargo run" suffix)))
        (rustic-run-cargo-command cmd)))))

(defun rustic-cargo-run-current-with-args ()
  "Run the current buffer if bin, else default, with prompted args."
  (interactive)
  (rustic-cargo-run-current (read-string "Enter args: ")))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (set-popup-rule! "^\\*rustic-compilation*" :side 'bottom :size 0.45 :select t)
  (map! :map rustic-mode-map
        :localleader
        (:prefix ("b" . "build")
          :desc "cargo run" "r" #'rustic-cargo-run-current
          :desc "cargo run with args" "R" #'rustic-cargo-run-current-with-args
          ))
  )
