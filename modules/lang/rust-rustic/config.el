;;; lang/rust-rustic/config.el -*- lexical-binding: t; -*-

(use-package! rustic
  :config
  (set-docsets! 'rustic-mode "Rust")
  (set-formatter! 'rustic-format-buffer #'rustic-format-buffer
    :modes '(rustic-mode))
  (setq rustic-format-on-save nil
        rustic-indent-where-clause t
        rustic-indent-method-chain t))

(after! rustic
  (require 'smartparens-rust)

  (sp-with-modes '(rustic-mode)
    (sp-local-pair "'" "'"
                   :unless '(sp-in-comment-p sp-in-string-quotes-p sp-in-rust-lifetime-context)
                   :post-handlers'(:rem sp-escape-quotes-after-insert))
    (sp-local-pair "<" ">"
                   :when '(sp-rust-filter-angle-brackets)
                   :skip-match 'sp-rust-skip-match-angle-bracket))

  ;; Rust has no sexp suffices.  This fixes slurping
  ;; (|foo).bar -> (foo.bar)
  (add-to-list 'sp-sexp-suffix (list #'rustic-mode 'regexp ""))

  (defun rustic-setup-rls ()
    (require 'lsp)
    (require 'lsp-rust)
    (setq rustic-lsp-server 'rls
          rustic-analyzer-command '("rls"))
    (lsp)))
