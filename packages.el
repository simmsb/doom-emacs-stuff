;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! company-anaconda anaconda-mode ocamlformat code-review)

;; (package! evil-textobj-treesitter)

(package! outline-minor-faces)
(package! circadian)

(unpin! lsp-mode lsp-haskell rustic closql git-commit magit orgit-forge emacsql emacsql-sqlite-builtin)

(package! emacsql-sqlite-builtin
  :recipe (:host github :repo "magit/emacsql"))
(package! emacsql
  :recipe (:host github :repo "magit/emacsql"))

(package! magit :recipe (:host github :repo "magit/magit")
  :pin "24f64fd4f8ed4a4a302fd9227febad63507d7287")

(package! forge :recipe (:host github :repo "magit/forge")
  :pin "ec68fcd778f6b3dc6100498aea790457d2fc98f6")

(package! lsp-haskell :recipe (:host github :repo "emacs-lsp/lsp-haskell"))

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"))
 

(package! zone-matrix-wake-up
  :recipe (:host github :repo "vreeze/zone-matrix-wake-up"))

(package! geros :recipe (:host github :repo "nitros12/geros"))

(package! names)

(package! el-patch
  :recipe (:host github :repo "raxod502/el-patch")
  :pin "4a4e040fcede0c320e860571d5e96100cac05bb5")

(package! fzf-native
  :recipe (:host github :repo "simmsb/fzf-native"
                 :files (:defaults "bin")))
(package! fussy
  :recipe (:host github :repo "jojojames/fussy"
           :files (:defaults "bin")))

(setq ON-LAPTOP (string= (system-name) "laptop"))
(setq ON-DESKTOP (string= (system-name) "home"))

(if ON-LAPTOP
    (disable-packages! lsp-haskell flycheck-haskell)
  (package! discord-emacs :recipe (:host github :repo "simmsb/discord-emacs.el")))

(package! citeproc)
;; (package! org-ref)

;;(package! elpy)
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
