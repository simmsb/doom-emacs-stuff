;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

(package! member-functions :recipe (:fetcher wiki :filename "member-functions.el"))

(package! xclip :disable t)

(package! doom-modeline :recipe (:fetcher github :repo "seagle0128/doom-modeline"))
(package! lsp-mode      :recipe (:fetcher github :repo "emacs-lsp/lsp-mode"))
(package! component     :recipe (:fetcher github :repo "nitros12/component-el"))
(package! twooter       :recipe (:fetcher github :repo "nitros12/twooter.el"))
(package! geros         :recipe (:fetcher github :repo "nitros12/geros"))

;; (package! twooter       :recipe (:fetcher file :path "~/dev/twooter.el"))
;; (package! component     :recipe (:fetcher file :path "~/dev/component-el"))
(package! names)
(package! aio)


(package! company)
(package! company-quickhelp)
(package! graphviz-dot-mode)

(when (string= (system-name) "laptop")
  (eval '(progn (package! lsp-haskell :disable t)
                (package! flycheck-haskell :disable t))))

(unless (string= (system-name) "laptop")
  (eval '(package! discord-emacs :recipe (:fetcher github :repo "nitros12/discord-emacs.el"))))


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
