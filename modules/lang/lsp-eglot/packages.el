;; -*- no-byte-compile: t; -*-
;;; lang/lsp-eglot/packages.el

(package! eglot)

(when (featurep! +rust)
  (package! company-racer :disable t)
  (package! flycheck-rust :disable t))

(when (featurep! +python)
  (package! company-anaconda :disable t)
  (package! anaconda-mode :disable t))
