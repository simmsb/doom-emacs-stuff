;;; private/ben/init.el -*- lexical-binding: t; -*-

(require 'discord-ipc)
(require 'whitespace-cleanup-mode)
(require 'rainbow-identifiers)
(require 'color-theme-sanityinc-tomorrow)
(require 'elpy)

(setq user-mail-address "ben@bensimms.moe"
      user-full-name "Ben Simms")

(fringe-mode 8)
(show-paren-mode 1)
(setq show-paren-style 'expression
      show-paren-delay 0)

(setq default-directory "~/dev/")

(def-package-hook! solaire-mode :disable)

(setq frame-title-format (list (user-login-name) "@" (system-name)))

(def-package-hook! anaconda-mode
  :pre-config
  (set! :popup "*anaconda-mode*" :size 10 :noselect t :autoclose t :autokill t)
  (map! :map anaconda-mode-map :m "gd" #'anaconda-mode-find-definitions)
  (advice-add #'anaconda-mode-doc-buffer :after #'doom*anaconda-mode-doc-buffer)
  nil)

(global-whitespace-cleanup-mode)

(pcase (system-name)
  ("laptop"
   (setq doom-font (font-spec :family "Fira Mono"
                              :size 12) ; size 19 on pc, 12 on laptop
         doom-unicode-font (font-spec :family "Fira Mono")))
  (_
   (run-at-time "1 min" nil #'discord-ipc-run "384815451978334208")
   (add-to-list 'exec-path "/home/ben/.local/bin/")
   (setq doom-font (font-spec :family "Fira Mono"
                              :size 19) ; size 19 on pc, 12 on laptop
         doom-unicode-font (font-spec :family "Fira Mono"))))
