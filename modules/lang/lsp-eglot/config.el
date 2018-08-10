;;; lang/lsp-eglot/config.el -*- lexical-binding: t; -*-

(def-package! eglot
  :commands (eglot-ensure))

(when (featurep! +rust)
  (add-hook! rust-mode #'eglot-ensure))
(when (featurep! +python)
  (add-hook! python-mode #'eglot-ensure))
