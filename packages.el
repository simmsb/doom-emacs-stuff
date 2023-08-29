;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! anaconda-mode ocamlformat code-review)

;; (package! evil-textobj-treesitter)

(package! outline-minor-faces)

(unpin! lsp-mode lsp-haskell rustic magit forge emacsql closql git-commit orgit-forge emacsql emacsql-sqlite-builtin haskell-mode)

(package! emacsql-sqlite-builtin
  :recipe (:host github :repo "magit/emacsql"))
(package! emacsql
  :recipe (:host github :repo "magit/emacsql"))

(package! magit :recipe (:host github :repo "simmsb/magit"))
 

(package! forge :recipe (:host github :repo "magit/forge"))
 

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

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars"))

(package! rustic
  :recipe (:host github :repo "deftsp/rustic")
  :pin "c745232b1bf7afc027a0d9c711ad86bbe29ed3bf")

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
