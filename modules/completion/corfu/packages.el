;; -*- no-byte-compile: t; -*-
;;; tools/grip/packages.el

(package! fzf-native
  :recipe (:host github :repo "dangduc/fzf-native"
           :files (:defaults "bin")))
(package! fussy
  :recipe (:host github :repo "jojojames/fussy"
           :files (:defaults "bin")))
(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))
(package! kind-icon)
(package! orderless)
(package! cape)
(package! popon
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
