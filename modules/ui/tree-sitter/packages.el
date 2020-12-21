;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el


(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "d6e98defcaf1d928bb9a1849f45e85d669925c26")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "d6e98defcaf1d928bb9a1849f45e85d669925c26")
