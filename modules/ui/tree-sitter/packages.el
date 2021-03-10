;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el


(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix)))

(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix)))
