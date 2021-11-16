;; -*- no-byte-compile: t; -*-
;;; init.el -*- lexical-binding: t; -*-

(setq comp-speed 2)

(setq ON-DESKTOP (string= (system-name) "home"))
(setq ON-LAPTOP (string= (system-name) "laptop"))

(doom! `(:editor
         file-templates
         snippets
         (evil +everywhere)
         format
         multiple-cursors
         (parinfer +rust)
         rotate-text
         fold

         :os
         ,@(when IS-MAC
             '(macos))

         :completion
         ;; (company +tng)
         company
         ;(ivy +fuzzy +prescient)
         (vertico)

         :ui
         workspaces
         hydra
         vc-gutter
         (popup
          +all
          +defaults)
         ;ligatures
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
         (emoji +unicode)

         :emacs
         undo
         (ibuffer +icons)
         vc
         (dired +icons)
         electric

         :term
         eshell

         :tools
         tree-sitter
         biblio
         docker
         (eval +overlay)
         (lookup +dictionary)
         lsp
         make
         editorconfig
         (magit +forge)
         direnv
         gist
         pass
         pdf
         rgb
         upload

         :checkers
         (spell +enchant +flyspell)
         (syntax +childframe)

         :lang
         (go +lsp)
         ;; poly
         (clojure +lsp)
         plantuml
         pest
         yaml
         json
         p4
         jade
         sourcepawn
         ebnf
         ;; (scheme +racket)
         racket
         (cc +lsp)
         data
         (elixir +lsp)
         emacs-lisp
         (haskell +lsp)
         (java +lsp)
         (kotlin +lsp)
         (javascript +lsp)
         (latex +pdf-tools)
         markdown
         (csharp +lsp)

         (org
          +gnuplot
          +dragndrop
          +pandoc
          +present
          +hugo
          +pretty)
         (python +lsp +poetry +pyright)
         rest
         (rust +lsp)
         (sh +fish)
         web

         ;; ,@(when ON-DESKTOP
         ;;     '(:email
         ;;       (mu4e +gmail)))

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

;; (setq +mu4e-backend 'offlineimap)

(setq custom-safe-themes t)

(fringe-mode 4)

;; (show-paren-mode t)
;; (setq show-paren-style 'mixed
;;       show-paren-delay 0)

(setq default-directory "~/dev/")

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

(setq doom-theme 'doom-flatwhite)

(pcase (system-name)
  ("home"
   (setq doom-font (font-spec :family "Fira Code" :size 16)
         doom-big-font (font-spec :family "Fira Code" :size 22)
         doom-variable-pitch-font (font-spec :family "Fira Sans")
         doom-unicode-font (font-spec :family "Twemoji")))
  ("laptop"
   (toggle-frame-maximized)
   (setq doom-font (font-spec :family "Fira Mono" :size 14)
         doom-big-font (font-spec :family "Fira Mono" :size 18)))
  ("work-desktop"
   (setq doom-font (font-spec :family "Fira Code" :size 14)
         doom-big-font (font-spec :family "Fira Code" :size 18))))

;; (setq lsp-print-io t)
