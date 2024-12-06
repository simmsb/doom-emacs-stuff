;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! anaconda-mode ocamlformat code-review writegood-mode flyspell)

;; (package! evil-textobj-treesitter)
(package! typst-ts-mode :recipe (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el")))

(package! outline-minor-faces)

(unpin! lsp-mode lsp-haskell rustic
        git-commit
        orgit-forge
        haskell-mode tree-sitter tree-sitter-langs
        tree-sitter-indent
        consult
        vertico
        corfu
        pdf-tools
        parinfer-rust-mode
        magit transient forge)

(package! pdf-tools :recipe (:host github :repo "vedang/pdf-tools"))

(package! parinfer-rust-mode
  :recipe (:host github :repo "justinbarclay/parinfer-rust-mode"))

(package! auth-source-1password
  :recipe (:host github :repo "dlobraico/auth-source-1password"))

;(package! emacsql-sqlite-builtin
;  :recipe (:host github :repo "magit/emacsql"))
;(package! emacsql
;  :recipe (:host github :repo "magit/emacsql"))
;(package! magit :recipe (:host github :repo "simmsb/magit" :branch "aaa") :pin "282cc8bc2b100c14dfa8350ec0159ec3bbdc7916")
(package! magit :recipe (:host github :repo "magit/magit") :pin "f2a61334430291d2162a68138c95ab310a8557f1")
(package! transient :recipe (:host github :repo "magit/transient"))
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

(package! flx-rs
  :recipe (:host github :repo "jcs-elpa/flx-rs"
                 :files (:defaults "bin")))

(package! fussy
  :recipe (:host github :repo "jojojames/fussy"
           :files (:defaults "bin")))

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars"))

(package! rustic
  :recipe (:host github :repo "emacs-rustic/rustic"))

(package! boxes)

(setq ON-LAPTOP (string= (system-name) "laptop"))
(setq ON-DESKTOP (string= (system-name) "home"))

(package! ultra-scroll-mac
  :recipe (:host github :repo "jdtsmith/ultra-scroll-mac"))

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

(package! auto-dark)

(package! affe)

(package! scad-mode)

;; (package! lsp-copilot :recipe (:host github :repo "jadestrong/lsp-copilot"
;;                                :files ("lsp-copilot.el" "lsp-copilot")
;;                                :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-copilot" "./"))))
