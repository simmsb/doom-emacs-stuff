;;; private/sourcepawn/config.el -*- lexical-binding: t; -*-

(use-package! sourcepawn-mode
  :config (setq default-tab-width 4)
  :load-path "~/.doom.d/local/sourcepawn-mode"
  :mode "\\.sp$")
