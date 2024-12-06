;; -*- no-byte-compile: t; -*-
;;; init.el -*- lexical-binding: t; -*-

(setq comp-speed 2)

(setenv "LSP_USE_PLISTS" "1")

;; (appendq! doom-env-deny '("^ALACRITTY" "^STARSHIP" "^ATUIN" "^ZELLIJ" "^SHELL"))

(defadvice! straight-use-recipes-ignore-nongnu-elpa-a (fn recipe)
  :around #'straight-use-recipes
  (unless (eq 'nongnu-elpa (car recipe))
    (funcall fn recipe)))


(setq ON-DESKTOP (string= (system-name) "home"))
(setq ON-LAPTOP (string= (system-name) "laptop"))


(doom! `(:editor
         file-templates
         snippets
         (evil +everywhere)
         (format +lsp)
         multiple-cursors
         (parinfer +rust)
         rotate-text
         fold
         word-wrap

         :os
         ,@(when IS-MAC
             '(macos))

         :completion
         ;; (company +tng)
         ;; company
         (corfu +icons +orderless +dabbrev +dict)
         ;(ivy +fuzzy +prescient)
         (vertico)

         :ui
         zen
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
         treemacs
         vi-tilde-fringe
         window-select

         :emacs
         undo
         (ibuffer +icons)
         vc
         (dired +icons +dirvish)
         electric

         :term
         eshell
         vterm
         shell

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
         ;; (pass +auth)
         pdf
         rgb
         upload
         (terraform +lsp)

         :checkers
         jinx
         (syntax +childframe)
         grammar

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
         (yaml +lsp)
         (json +lsp)
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
         ;(latex +pdf-tools)
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
         (rust +lsp)
         (sh +fish +tree-sitter +lsp)
         (web +tree-sitter +lsp)

         ;; ,@(when ON-DESKTOP
         ;;     '(:email
         ;;       (mu4e +gmail)))

         ;; Applications are complex and opinionated modules that transform Emacs
         ;; toward a specific purpose. They may have additional dependencies and
         ;; should be loaded late.
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

(setq gcmh-high-cons-threshold (* 256 1024 1024)
      read-process-output-max (* 1024 1024))

;; (show-paren-mode t)
;; (setq show-paren-style 'mixed
;;       show-paren-delay 0)

(setq default-directory "~/dev/")

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (json-mode . json-ts-mode)))
        ;; (tsx-ts-mode . typescript-tsx-mode)))
