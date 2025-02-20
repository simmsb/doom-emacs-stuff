;;; lang/numbat-ts-mode/config.el -*- lexical-binding: t; -*-

(require 'treesit)

(use-package! numbat-ts-mode
  :load-path "~/.doom.d/local/numbat-ts-mode"
  :init
  (add-to-list 'treesit-language-source-alist '(numbat . ("https://github.com/simmsb/tree-sitter-numbat")))
  :mode "\\.nbt$")
