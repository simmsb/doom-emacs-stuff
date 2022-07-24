;; -*- no-byte-compile: t; -*-
;;; tools/grip/packages.el

(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))
(package! kind-icon)
(package! orderless)
(package! corfu-doc
  :recipe (:host github :repo "galeo/corfu-doc"))
(package! cape)
(package! popon
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
