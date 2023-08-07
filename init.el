;; -*- no-byte-compile: t; -*-
;;; init.el -*- lexical-binding: t; -*-

(setq comp-speed 2)

(setenv "LSP_USE_PLISTS" "1")


;; (appendq! doom-env-deny '("^ALACRITTY" "^STARSHIP" "^ATUIN" "^ZELLIJ" "^SHELL"))


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
         ;; company
         (corfu +icons +orderless)
         ;(ivy +fuzzy +prescient)
         (vertico)

         :ui
         workspaces
         (vc-gutter +pretty +diff-hl)
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
         (treemacs +lsp)
         vi-tilde-fringe
         window-select
         (emoji +unicode)

         :emacs
         undo
         (ibuffer +icons)
         vc
         (dired +icons +dirvish)
         electric

         :term
         eshell

         :tools
         tree-sitter
         biblio
         ;; docker
         (eval +overlay)
         (lookup +dictionary)
         lsp
         make
         editorconfig
         (magit +forge)
         direnv
         gist
         (pass +auth)
         pdf
         rgb
         upload
         (terraform +lsp)

         :checkers
         jinx
         (syntax +childframe)

         :lang
         art
         (zig +tree-sitter +lsp)
         (nix +tree-sitter +lsp)
         (ocaml +tree-sitter +lsp)
         (go +lsp +tree-sitter)
         ;; poly
         ;; (clojure +lsp)
         plantuml
         pest
         (yaml +lsp +tree-sitter)
         (json +lsp +tree-sitter)
         ;p4
         ;jade
         sourcepawn
         ebnf
         ;; (scheme +racket)
         racket
         (cc +lsp +tree-sitter)
         data
         (elixir +lsp +tree-sitter)
         emacs-lisp
         (haskell +lsp)
         ;(java +lsp)
         ;(kotlin +lsp)
         (javascript +lsp +tree-sitter)
         (latex +pdf-tools)
         markdown
         ;(csharp +lsp)
         hcl
         just

         (org
          +gnuplot
          +dragndrop
          +pandoc
          +present
          +hugo
          +pretty)
         (python +lsp +pyright +tree-sitter)
         rest
         (rust +lsp +tree-sitter)
         (sh +fish +tree-sitter +lsp)
         (web +tree-sitter +lsp)

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
