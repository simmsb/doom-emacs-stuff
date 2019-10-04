;;; tools/grip/config.el -*- lexical-binding: t; -*-

(use-package! grip-mode
  :hook ((markdown-mode org-mode) . grip-mode))
