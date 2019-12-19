;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; # General / editor

(setq lsp-log-io t)

;; Use , as shortcut for SPC-m
(setq doom-localleader-key ",")

;; Bigger font for tired eyes
(setq doom-font (font-spec :family "Menlo" :size 18))

;; prefer ~/ to navigate to home in ivy file searches rather than just ~
(after! ivy
  (setq ivy-magic-tilde nil))

;; Use a subset of lispy keythemes, as the defaults are too
;; aggressive.
(after! lispy
  (lispy-set-key-theme '(lispy c-digits paredit)))


;; # Clojure

;; Turn on clj-kondo linting because it's great
(require 'flycheck-clj-kondo)

(after! cider
  ;; Popup cider panels to the right in vsplit
  (set-popup-rule! "^\\*cider-repl" :side 'right :quit nil :size 0.5 :modeline t)
  (set-popup-rule! "^\\*cider-scratch\\*" :side 'right :quit nil :size 0.5 :modeline t)

  ;; fipp is much faster than the default printer
  (setq cider-print-fn 'fipp))

(after! lispy
  ;; The default [ and ] are navigation, which makes
  ;; clojure difficult to type.
  (define-key lispy-mode-map (kbd "[") 'lispy-open-square)
  (define-key lispy-mode-map (kbd "]") 'lispy-close-square)
  (define-key lispy-mode-map (kbd "}") 'lispy-close-curly))

;; Use paredit. smartparents is still not quite right.
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)


;; # Rust

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

(setq lsp-rust-server 'rust-analyzer)
(after! rustic
  (set-popup-rule! "^\\*rustic-compilation*" :side 'bottom :size 0.7 :select t)
  (map! :map rustic-mode-map
        :leader
        (:prefix ("c" . "code")
          :desc "LSP code action" "a" #'lsp-execute-code-action
          :desc "Inline code hints" "h" #'lsp-rust-analyzer-inlay-hints-mode)

        :localleader
        (:prefix ("b" . "build")
          :desc "cargo run" "r" #'rustic-cargo-run-current
          :desc "cargo run with args" "R" #'rustic-cargo-run-current-with-args))
  )

;; rust-analyzer is our preferred lsp server for rust. Unfortunately, it doesn't
;; actually produce any diagnostics right now (whereas rls does). Instead, it
;; relies on clients to run `cargo clippy` themselves. It comes with a vscode
;; client that does this, which is also used by the vim lsp rust-analyzer
;; client, but the emacs one (defined by the lsp-mode package) does not work
;; with it.
;;
;; lsp mode and the doom emacs lsp integration selects the lsp-ui flycheck
;; checker automatically. This checker relies on the lsp server to provide
;; diagnostics. Since rust-analyzer does not provide diagnostics, we want to
;; disable this checker for rust files. I do not see a way to set this locally,
;; so we set it globally (just using `(add-hook 'rustic-mode-hook ...)` doesn't
;; work, as it's immediately overwritten elsewhere).
(after! lsp-ui
  (setq lsp-prefer-flymake :none))
