;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(disable-packages! anaconda-mode ocamlformat code-review writegood-mode flyspell)

;; (package! evil-textobj-treesitter)
(package! typst-ts-mode :recipe (:type git :host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el")))

(package! outline-minor-faces)

(unpin! lsp-mode lsp-haskell rustic
        orgit-forge
        haskell-mode tree-sitter tree-sitter-langs
        consult
        vertico
        corfu
        pdf-tools
        parinfer-rust-mode
        magit transient forge
        ultra-scroll
        diff-hl)

;; (package! pdf-tools :recipe (:host github :repo "vedang/pdf-tools"))
(package! pdf-tools :recipe (:host github :repo "aikrahguzar/pdf-tools"
                                   :branch "child-frame-preview"))


(package! parinfer-rust-mode
  :recipe (:host github :repo "justinbarclay/parinfer-rust-mode"))

(package! auth-source-1password
  :recipe (:host github :repo "dlobraico/auth-source-1password"))

;(package! emacsql-sqlite-builtin
;  :recipe (:host github :repo "magit/emacsql"))
;(package! emacsql
;  :recipe (:host github :repo "magit/emacsql"))
;(package! magit :recipe (:host github :repo "simmsb/magit" :branch "aaa") :pin "282cc8bc2b100c14dfa8350ec0159ec3bbdc7916")

;(package! jj-mode :recipe (:host github :repo "bolivier/jj-mode.el"))
;; (package! vc-jj)
;; (package! strie)

(package! magit :recipe (:host github :repo "magit/magit"))
(package! transient :recipe (:host github :repo "magit/transient"))
(package! forge :recipe (:host github :repo "magit/forge"))
(package! magit-prime
  :recipe (:host github :repo "Azkae/magit-prime"))

(package! nushell-ts-mode :recipe (:host github :repo "herbertjones/nushell-ts-mode"))

;; (package! jujutsu :recipe (:host github :repo "bennyandresen/jujutsu.el"))

(package! lsp-haskell :recipe (:host github :repo "magthe/lsp-haskell"))
;; (package! haskell-ts-mode
;;   :recipe (
;;            :host codeberg
;;            :repo "pranshu/haskell-ts-mode"
;;            :branch "main"))

;(package! lsp-haskell :recipe (:host github :repo "emacs-lsp/lsp-haskell"))

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"))


(package! zone-matrix-wake-up
  :recipe (:host github :repo "vreeze/zone-matrix-wake-up"))

(package! geros :recipe (:host github :repo "nitros12/geros"))

(package! names)

(package! el-patch
  :recipe (:host github :repo "raxod502/el-patch")
  :pin "4a4e040fcede0c320e860571d5e96100cac05bb5")

;; (package! flx-rs
;;   :recipe (:host github :repo "jcs-elpa/flx-rs"
;;                  :files (:defaults "bin")))

;; (package! fzf-native
;;   :recipe (:repo "dangduc/fzf-native"
;;            :host github
;;            :files (:defaults "bin")))

(package! hotfuzz :type 'built-in :built-in t)
(package! jinx :type 'built-in :built-in t)
(package! vterm :type 'built-in :built-in t)

(package! nucleo
  :recipe (:host github :repo "simmsb/emacs-nucleo"
           :files (:defaults "nucleo-module.dylib")))


;; (package! fussy
;;   :recipe (:host github :repo "jojojames/fussy"
;;            :files (:defaults "bin")))

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars"))

(package! rustic
  :recipe (:host github :repo "emacs-rustic/rustic"))

(package! boxes)

;; (package! ultra-scroll
;;   :recipe (:host github :repo "jdtsmith/ultra-scroll"))

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

(package! f)

(package! auto-dark)

(package! affe)

(package! scad-mode)

;; (package! difftastic
;;   :recipe (:host github :repo "pkryger/difftastic.el"))

;; (package! lsp-copilot :recipe (:host github :repo "jadestrong/lsp-copilot"
;;                                :files ("lsp-copilot.el" "lsp-copilot")
;;                                :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-copilot" "./"))))
(package! compile-angel)

(package! org-flyimage
  :recipe (:host github :repo "misohena/org-inline-image-fix"))
(package! org-limit-image-size
  :recipe (:host github :repo "misohena/org-inline-image-fix"))
(package! org-tidy)
;; (package! org-hide-tags
;;   :recipe (:host github :repo "amno1/org-hide-tags"))

(package! ox-typst
  :recipe (:host github :repo "jmpunkt/ox-typst"))

(package! rainbow-delimiters)
(package! ast-grep :recipe (:host github :repo "SunskyXH/ast-grep.el"))
