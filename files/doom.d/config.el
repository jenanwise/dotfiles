;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Don't prompt when exiting
(setq confirm-kill-emacs nil)

;; Use , as shortcut for SPC-m
(setq doom-localleader-key ",")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jenan Wise"
      user-mail-address "jenan@jenanwise.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Input Mono" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Prefer ~/ to navigate to home in ivy file searches rather than just ~
(after! ivy
  (setq ivy-magic-tilde nil))

;; Use a subset of lispy keythemes, as the defaults are too
;; aggressive.
(after! lispy
  (lispy-set-key-theme '(lispy c-digits paredit)))


;; Train myself to use jk instead of escape to exit insert mode.
(defun no-esc ()
  (interactive)
  (message "Use jk to exit insert mode!"))
(map! :i "<escape>" #'no-esc)

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
(add-hook 'rust-mode-hook #'paredit-mode)
(add-hook 'rustic-mode-hook #'paredit-mode)


;; # Rust

;; By default, paredit puts a space before all delimeters.
;; In Rust, we actually never want it to do that.
(defun set-up-paredit-delimeter-prefs ()
  (when (member major-mode '(rustic-mode rust-mode))
    (add-to-list 'paredit-space-for-delimiter-predicates
                 (lambda (_endp _delimeter) nil))))
(add-hook 'paredit-mode-hook #'set-up-paredit-delimeter-prefs)


;; Teach paredit about `|foo|`, which Rust uses for lambdas.
;; AFAICT I am the only person in the world using paredit+Rust...

(defun paredit-open-or-close-vert (&optional n)
  (interactive "P")
  (cond
   ;; in a string or comment, insert a normal literal "|"
   ((or (paredit-in-comment-p) (paredit-in-string-p))
    (insert ?\|))
   ;; if the char at the point is a "|", just move
   ;; forward, as we're likely at the closing "|"
   ;; of a "|" pair. paredit's `paredit-doublequote'
   ;; is smarter than this and tests to see if it's
   ;; actually inside a string first, but we don't need
   ;; to do that.
   ((equal "|" (string (char-after (point))))
    (forward-char))
   ;; else we're in normal code; go ahead and insert a "||",
   ;; which is done by calling `paredit-open-vert', which is
   ;; defined when calling `define-paredit-pair'
   (t
    (paredit-open-vert))))
(after! paredit
  (define-paredit-pair ?\| ?\| "vert")
  (define-key rustic-mode-map (kbd "|") #'paredit-open-or-close-vert))

;; Somewhere in doom<>rustic<>lsp-mode<>rust-analyzer, something is messed up
;; with the signature auto overlay. By default, it pops up a huge pane
;; constantly while typing, so we disable it.
(setq-hook! 'rustic-mode-hook lsp-signature-auto-activate nil)

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

(setq rustic-lsp-server 'rust-analyzer)

;; By default, lsp-ui-sideline only shows "quickfix.*" and "refactor.*" actions.
;; However, rust-analyzer produces empty-string actions and "source.*" actions.
;; There is no reason to filter them, so just show them all.
;; https://github.com/emacs-lsp/lsp-ui/issues/443
(setq lsp-ui-sideline-actions-kind-regex ".*")

;; Allow turning on inlay hints. Without this settings,
;; `lsp-rust-analyzer-inlay-hints-mode' doesn't actually do anything.
(setq lsp-rust-analyzer-server-display-inlay-hints t)

(after! rustic
  ;;(set-popup-rule! "^\\*rustic-compilation*" :side 'bottom :size 0.7 :select t)
  (map! :map rustic-mode-map
        :leader
        (:prefix ("c" . "code")
          :desc "LSP code action" "a" #'lsp-execute-code-action
          :desc "Inline code hints" "h" #'lsp-rust-analyzer-inlay-hints-mode)

        :localleader
        (:prefix ("b" . "build")
          :desc "cargo run" "r" #'rustic-cargo-run-current
          :desc "cargo run with args" "R" #'rustic-cargo-run-current-with-args)))

;; # Web
(after! web-mode
  ;; The tidy formater is garbage. Remove it.
  (set-formatter! 'html-tidy nil)

  ;; Auto close after <div>
  (setq web-mode-auto-close-style 2)

  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  )
