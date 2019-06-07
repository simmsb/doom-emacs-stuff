;;; lang/rust-rustic/config.el -*- lexical-binding: t; -*-

(def-package! rustic
  :config
  (set-docsets! 'rustic-mode "Rust")
  (set-formatter! 'rustic-format-buffer #'rustic-format-buffer
    :modes '(rustic-mode))
  (setq rustic-format-on-save nil
        rustic-indent-where-clause t
        rustic-indent-method-chain t))
