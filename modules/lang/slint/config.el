;;; lang/slint/config.el -*- lexical-binding: t; -*-

(use-package! slint-mode
  :defer t
  :config
  (when (modulep! +lsp)
    (add-hook 'slint-mode-local-vars-hook #'lsp! 'append)))
