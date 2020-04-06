;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(package! magit-section)

(disable-packages! company-anaconda anaconda-mode)

(package! doom-modeline :recipe (:host github :repo "seagle0128/doom-modeline"))
;(package! lsp-mode      :recipe (:host github :repo "emacs-lsp/lsp-mode"))
(package! component     :recipe (:host github :repo "nitros12/component-el"))
(package! twooter       :recipe (:host github :repo "nitros12/twooter.el"))
(package! geros         :recipe (:host github :repo "nitros12/geros"))

;; (package! twooter       :recipe (:host file :path "~/dev/twooter.el"))
;; (package! component     :recipe (:host file :path "~/dev/component-el"))
(package! names)
(package! aio)


(package! company)
(package! company-quickhelp)
(package! graphviz-dot-mode)

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
(package! slack)
(package! sqlup-mode)
(package! org-sticky-header)
(package! smart-hungry-delete)
(package! backline)
(package! esh-autosuggest)
(when ON-DESKTOP
  (package! mu4e-alert))

(package! f)
