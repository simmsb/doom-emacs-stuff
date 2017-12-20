;;; private/ben/config.el -*- lexical-binding: t; -*-

(load! +bindings)

(setq doom-font (font-spec :family "Fira Mono" :size 19)
      doom-unicode-font (font-spec :family "Fira Mono"))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-quickhelp-mode 1
        company-quickhelp-delay 0.0)
  (global-company-mode))

(after! rainbow-identifiers
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(setq doom-line-numbers-style nil)
(setq doom-theme nil)

;(set! :company-backend 'python-mode '(company-anaconda))

(after! color-theme-sanityinc-tomorrow
  (color-theme-sanityinc-tomorrow-night))

(setq doom-neotree-file-icons t)

(after! company-quickhelp
  (company-quickhelp-mode 1))

(after! elpy
  (elpy-enable))
