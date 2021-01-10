;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el


(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "3dd686565b222fb00ff54b374333b48598f5317c")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "3dd686565b222fb00ff54b374333b48598f5317c")
