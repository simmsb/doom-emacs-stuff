;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(package! doom-modeline :recipe (:host github :repo "seagle0128/doom-modeline"))
(package! lsp-mode      :recipe (:host github :repo "emacs-lsp/lsp-mode"))
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

(when (string= (system-name) "laptop")
  (eval '(progn (package! lsp-haskell :disable t)
                (package! flycheck-haskell :disable t))))

(package! discord-emacs :recipe (:host github :repo "nitros12/discord-emacs.el"
                        :disable (string= (system-name) "laptop")))


;;(package! elpy)
(package! github-browse-file)
(package! github-review)
(package! py-isort)
(package! evil-multiedit)
(package! disable-mouse)
(package! clang-format)
(package! popup-kill-ring)
(package! company-math)
(package! transpose-frame)
(package! drag-stuff)
(package! discover-my-major)
(package! evil-lion)
(package! slack)
(package! emojify)
(package! anzu)
(package! evil-anzu)
(package! org-download)
(package! sqlup-mode)
(package! org-sticky-header)
(package! smart-hungry-delete)
(package! string-inflection)
(package! backline)
(package! esh-autosuggest)

(package! f)
(package! ox-hugo)
