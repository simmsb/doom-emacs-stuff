;;; private/ben/init.el -*- lexical-binding: t; -*-

(setq user-mail-address "ben@bensimms.moe"
      user-full-name "Ben Simms")

(load-file "~/dev/discord-emacs/discord-emacs.el")

;; (after! flycheck
;;   (add-hook! 'python-mode-hook (lambda ()
;;                                  (setq flycheck-checker 'python-pylint
;;                                        flycheck-pylintrc "~/.pylintrc"))))

(fringe-mode 8)
(show-paren-mode 1)
(setq show-paren-style 'expression
      show-paren-delay 0)

(setq default-directory "~/dev/")

(run-at-time "1 min" nil discord-ipc-run "384815451978334208")

(def-package-hook! solaire-mode :disable)

(setq frame-title-format (list (user-login-name) "@" (system-name)))

