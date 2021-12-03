;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! company-anaconda anaconda-mode ocamlformat)

;; (package! evil-textobj-treesitter)

(package! circadian)
(package! magit-libgit)
(package! libgit :recipe (:host github :repo "magit/libegit2"))
(package! lsp-mode :recipe (:host github :repo "emacs-lsp/lsp-mode"))
(package! lsp-haskell :recipe (:host github :repo "emacs-lsp/lsp-haskell"))

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces")
  :pin "ccebbdda0c3f8b5c6e5857bdd7f5a49d4739c904")

(package! geros :recipe (:host github :repo "nitros12/geros"))

(package! mermaid :recipe (:host github :repo "abrochard/mermaid-mode"))

(package! names)

(package! el-patch)

(setq ON-LAPTOP (string= (system-name) "laptop"))
(setq ON-DESKTOP (string= (system-name) "home"))

(if ON-LAPTOP
    (disable-packages! lsp-haskell flycheck-haskell)
  (package! discord-emacs :recipe (:host github :repo "simmsb/discord-emacs.el")))

(package! citeproc)
;; (package! org-ref)

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
