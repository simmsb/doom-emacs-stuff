;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! company-anaconda anaconda-mode)

(package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))

(package! geros       :recipe (:host github :repo "nitros12/geros"))
(package! lsp-haskell :recipe (:host github :repo "shaunplee/lsp-haskell"))

(package! names)

(setq ON-LAPTOP (string= (system-name) "laptop"))
(setq ON-DESKTOP (string= (system-name) "home"))

(if ON-LAPTOP
    (disable-packages! lsp-haskell flycheck-haskell)
  (package! discord-emacs :recipe (:host github :repo "nitros12/discord-emacs.el")))

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
