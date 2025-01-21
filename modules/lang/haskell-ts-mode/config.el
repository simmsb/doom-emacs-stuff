;;; lang/haskell-ts-mode/config.el -*- lexical-binding: t; -*-


(use-package! haskell-ts-mode
  :load-path "~/.doom.d/local/haskell-ts-mode"
  :init
  (add-to-list 'major-mode-remap-alist '(haskell-mode . haskell-ts-mode))
  :config
  (setq haskell-ts-highlight-signature t
        haskell-ts-use-indent t)
  :mode "\\.hs$"
  :hook
  (haskell-ts-mode . lsp-deferred))
