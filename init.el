;;; private/ben/init.el -*- lexical-binding: t; -*-

(setq user-mail-address "ben@bensimms.moe"
      user-full-name "Ben Simms")

(setq auth-source-debug t)

(fringe-mode 8)
(show-paren-mode 1)
(setq show-paren-style 'expression
      show-paren-delay 0)

(setq default-directory "~/dev/")

(def-package-hook! solaire-mode :disable)

(setq frame-title-format (list (user-login-name) "@" (system-name)))

(setq +rust-src-dir (concat (replace-regexp-in-string
                             "\n\\'" ""
                             (shell-command-to-string "rustc --print sysroot"))
                            "/lib/rustlib/src"))

(def-package-hook! anaconda-mode
  :pre-config
  (set! :popup "*anaconda-mode*" :size 10 :noselect t :autoclose t :autokill t)
  (map! :map anaconda-mode-map :m "gd" #'anaconda-mode-find-definitions)
  (advice-add #'anaconda-mode-doc-buffer :after #'doom*anaconda-mode-doc-buffer)
  nil)

(pcase (system-name)
  ("laptop"
   (setq doom-font (font-spec :family "Fira Mono"
                              :size 12) ; size 19 on pc, 12 on laptop
         doom-unicode-font (font-spec :family "Fira Mono")))
  (_
   (add-to-list 'exec-path "/home/ben/.local/bin/")
   (add-to-list 'exec-path "/home/ben/.cargo/bin/")
   (setq doom-font (font-spec :family "Fira Mono"
                              :size 19) ; size 19 on pc, 12 on laptop
         doom-unicode-font (font-spec :family "Fira Mono"))))

(def-package-hook! magit
  :post-config
  (require 'pretty-magit)
  (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
  (pretty-magit "master"  ? (:box nil :height 1.0 :family "github-octicons") t)
  (pretty-magit "origin"  ? (:box nil :height 1.0 :family "github-octicons") t)
  t)
