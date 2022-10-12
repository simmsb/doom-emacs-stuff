;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! company-anaconda anaconda-mode ocamlformat)

;; (package! evil-textobj-treesitter)

(package! outline-minor-faces)
(package! circadian)
(unpin! lsp-mode lsp-haskell treemacs rustic)
(package! lsp-haskell :recipe (:host github :repo "emacs-lsp/lsp-haskell"))

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces")
  :pin "fe356507efb8c9b051c2a67213ba292f932a88bc")

(package! zone-matrix-wake-up
  :recipe (:host github :repo "vreeze/zone-matrix-wake-up"))

(package! geros :recipe (:host github :repo "nitros12/geros"))

(package! mermaid :recipe (:host github :repo "abrochard/mermaid-mode"))

(package! names)

(package! el-patch
  :recipe (:host github :repo "raxod502/el-patch")
  :pin "4a4e040fcede0c320e860571d5e96100cac05bb5")

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
