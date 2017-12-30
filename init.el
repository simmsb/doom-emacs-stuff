;;; private/ben/init.el -*- lexical-binding: t; -*-
(setq user-mail-address "ben@bensimms.moe"
      user-full-name "Ben Simms")

(unless (string= (system-name) "laptop")  ; dont use dipc on my laptop
  (require 'discord-ipc)
  (run-at-time "1 min" nil #'discord-ipc-run "384815451978334208"))

(fringe-mode 8)
(show-paren-mode 1)
(setq show-paren-style 'expression
      show-paren-delay 0)

(setq default-directory "~/dev/")

;;(def-package-hook! solaire-mode :disable)

(setq frame-title-format (list (user-login-name) "@" (system-name)))

(def-package-hook! anaconda-mode
  :pre-config
  (set! :popup "*anaconda-mode*" :size 10 :noselect t :autoclose t :autokill t)
  (map! :map anaconda-mode-map :m "gd" #'anaconda-mode-find-definitions)
  (advice-add #'anaconda-mode-doc-buffer :after #'doom*anaconda-mode-doc-buffer)
  nil)
