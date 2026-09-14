;; -*- no-byte-compile: t; -*-
;;; lang/curry-mode/packages.el

(package! lsp-mode)
(package! curry-mode
  :recipe (:host github
           :repo "tmcgilchrist/curry-mode"
           :branch "main"))
