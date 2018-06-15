;;; lang/geiser/config.el -*- lexical-binding: t; -*-

(def-package! geiser
  :commands geiser-mode geiser-company-backend
  :config
  (setq geiser-active-implementations '(racket chez))
  (set-company-backend! 'geiser-mode '(geiser-company-backend))
  :hook
  ((scheme-mode racket-mode) . geiser-mode))

(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))
