;;; lang/curry-mode/config.el -*- lexical-binding: t; -*-


(use-package! curry-mode
  :init
  (add-to-list 'major-mode-remap-alist '(haskell-mode . curry-mode))
  :mode "\\.hs$"
  :hook
  (curry-mode . lsp-deferred))
