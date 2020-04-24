;; -*- no-byte-compile: t; -*-
;;; init.el -*- lexical-binding: t; -*-

(setq ON-DESKTOP (string= (system-name) "home"))
(setq ON-LAPTOP (string= (system-name) "laptop"))

(doom! `(:editor
         file-templates
         snippets
         (evil +everywhere)
         format
         multiple-cursors
         parinfer
         rotate-text

         :completion
         ;; (company +tng)
         company
         (ivy +fuzzy +prescient)

         :ui
         workspaces
         hydra
         deft
         vc-gutter
         (popup
          +all
          +defaults)
         ,@(unless IS-MAC
             '((pretty-code
                +fira)))
         doom
         doom-dashboard
         modeline
         hl-todo
         nav-flash
         ophints
         treemacs
         vi-tilde-fringe
         window-select
         zen

         :emacs
         undo
         (ibuffer +icons)
         vc
         (dired +icons)
         electric

         :term
         eshell

         :tools
         docker
         (eval +overlay)
         (lookup +dictionary +offline)
         (lsp +peek)
         ,@(when IS-MAC
             '(macos))
         make
         editorconfig
         magit
         direnv
         gist
         pass
         pdf
         rgb
         upload

         :checkers
         grammar
         spell
         (syntax +childframe)

         :lang
         p4
         common-lisp
         jade
         sourcepawn
         ebnf
         scheme
         racket
         assembly
         (cc +lsp)
         data
         (elixir +lsp)
         elm
         emacs-lisp
         (haskell +lsp)
         hy
         (java +lsp)
         (javascript +lsp)

         (latex +pdf-tools)
         ledger
         lua
         markdown

         (org
          +gnuplot
          +dragndrop
          +pandoc
          +present
          +hugo)
         (python +lsp)
         rest
         (rust +lsp)
         (sh +fish)
         web

         ,@(when ON-DESKTOP
             '(:email
               (mu4e +gmail)))

         ;; Applications are complex and opinionated modules that transform Emacs
         ;; toward a specific purpose. They may have additional dependencies and
         ;; should be loaded late.
         :app
         irc
         :config
         ;; The default module set reasonable defaults for Emacs. It also provides
         ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
         ;; and additional ex commands for evil-mode. Use it as a reference for
         ;; your own modules.
         (default +bindings +smartparens)))

(setq user-mail-address "ben@bensimms.moe"
      user-full-name "Ben Simms")

(setq +mu4e-backend 'offlineimap)

(setq custom-safe-themes t)

(add-hook! text-mode #'auto-save-mode)

(fringe-mode 4)

(show-paren-mode t)
(setq show-paren-style 'mixed
      show-paren-delay 0)

(setq default-directory "~/dev/"
      default-tab-width 4)

(setq +rust-src-dir (concat (replace-regexp-in-string
                             "\n\\'" ""
                             (shell-command-to-string "rustc --print sysroot"))
                            "/lib/rustlib/src"))

(setq explicit-shell-file-name "/bin/bash"
      shell-file-name "/bin/bash")

(add-to-list 'exec-path "~/.local/bin/")
(add-to-list 'exec-path "~/.cargo/bin/")
(add-to-list 'exec-path "~/.npm-packages/bin/")

;; (setenv "GRADLE_HOME" "~/.gradle/wrapper/dists/gradle-5.1.1-bin")
;; (setenv "GRADLE_USER_HOME" "~/.gradle")

;; Does this work? idk
(setenv "PATH"
        (concat
         (expand-file-name "~/.npm-packages/bin/") path-separator
         (expand-file-name "~/.local/bin/") path-separator
         (expand-file-name "~/.cargo/bin/") path-separator
         (getenv "PATH")))

(pcase (system-name)
  ("home"
   (setq doom-theme 'doom-wilmersdorf)
   (setq doom-font (font-spec :family "Fira Mono" :size 17)
         doom-big-font (font-spec :family "Fira Mono" :size 23)))
  ("laptop"
   (toggle-frame-maximized)
   (setq doom-theme 'doom-tomorrow-night)
   (setq doom-font (font-spec :family "Fira Mono" :size 14)
         doom-big-font (font-spec :family "Fira Mono" :size 18)))
  ("work-desktop"
   (setq doom-theme 'doom-wilmersdorf)
   (setq doom-font (font-spec :family "Fira Mono" :size 14)
         doom-big-font (font-spec :family "Fira Mono" :size 18))))

;; (setq lsp-print-io t)
