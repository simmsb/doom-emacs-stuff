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
(package! company :recipe '(:files ("company.el" "company-yasnippet.el")) :pin "1005540b1cdf176cbcf893b2fa83d2075cbbe3ca")
