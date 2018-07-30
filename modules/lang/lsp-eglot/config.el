;;; lang/lsp-eglot/config.el -*- lexical-binding: t; -*-

(def-package! eglot
  :hook
  ((python-mode rust-mode) . eglot-ensure))
