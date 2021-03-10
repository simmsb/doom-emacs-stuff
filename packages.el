;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! company-anaconda anaconda-mode)

(package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))
(package! lsp-haskell :recipe (:host github :repo "emacs-lsp/lsp-haskell"))

(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

(package! geros :recipe (:host github :repo "nitros12/geros"))

(package! names)

(package! el-patch)

(setq ON-LAPTOP (string= (system-name) "laptop"))
(setq ON-DESKTOP (string= (system-name) "home"))

(if ON-LAPTOP
    (disable-packages! lsp-haskell flycheck-haskell)
  (package! discord-emacs :recipe (:host github :repo "nitros12/discord-emacs.el")))

(package! org-ref)

;;(package! elpy)
(package! typescript-mode)
(package! github-browse-file)
(package! github-review)
(package! disable-mouse)
(package! transpose-frame)
(package! evil-lion)
(package! sqlup-mode)
(package! org-sticky-header)
(package! backline)
(package! esh-autosuggest)
;; (when ON-DESKTOP
;;   (package! mu4e-alert))

(package! f)
