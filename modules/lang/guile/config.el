;;; lang/guile/config.el -*- lexical-binding: t; -*-

(def-package! geiser
  :commands geiser-mode
  :config
  (setq geiser-active-implementations '(guile))
  (set! :company-backend 'geiser-mode '(geiser-company))
  :hook
  (scheme-mode . geiser-mode))
