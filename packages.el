;; -*- no-byte-compile: t; -*-
;;; private/ben/packages.el

;; disabled
(package! anaconda-mode :disable t)

;; overidden
(package! alchemist :recipe (:fetcher github :repo "dsdshcym/alchemist.el"))
;; (package! alchemist :recipe (:fetcher github
;;                              :repo "trevoke/alchemist.el"
;;                              :branch "use-elixir-ls"))


;;(package! color-theme-sanityinc-tomorrow)
(package! rainbow-identifiers)
;;(package! solaire-mode :disable t)
;;(package! ivy-posframe :disable t)
(package! company)
(package! company-quickhelp)
(package! graphviz-dot-mode)

(unless (string= (system-name) "laptop")
  ;;(package! pretty-magit :recipe
  ;; (pretty-magit :url "https://gist.githubusercontent.com/nitros12/ed3a2265e9fabf39c46767ba0c65a85a/raw/58d5e2e858149548fa72e6060a5f00a7a46b10fa/pretty-magit.el"
  ;;               :fetcher url))
  (package! discord-emacs :recipe (:fetcher github :repo "nitros12/discord-emacs.el")))


;;(package! elpy)
(package! py-isort)
(package! evil-multiedit)
(package! disable-mouse)
(package! clang-format)
(package! popup-kill-ring)
(package! company-math)
(package! flycheck-credo)
(package! transpose-frame)
(package! pipenv)
(package! org-gcal :recipe (:fetcher github :repo "kidd/org-gcal.el"))
(package! drag-stuff)
(package! discover-my-major)

(package! f)
