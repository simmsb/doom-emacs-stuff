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
         rotate-text       ; cycle region at point between text candidates

         :completion
         company
         (ivy +fuzzy +prescient)      ; a search engine for love and life

         :ui
         workspaces
         deft
         vc-gutter
         (popup            ; tame sudden yet inevitable temporary windows
          +all             ; catch all popups that start with an asterix
          +defaults)       ; default popup rules
         ,@(unless IS-MAC
             '((pretty-code      ; replace bits of code with pretty symbols
                +fira)))
         doom              ; what makes DOOM look the way it does
         doom-dashboard    ; a nifty splash screen for Emacs
         modeline
         hl-todo           ; highlight TODO/FIXME/NOTE tags
         nav-flash         ; blink the current line after jumping
         unicode           ; extended unicode support for various languages
         ophints
         treemacs
         vi-tilde-fringe   ; fringe tildes to mark beyond EOB
         window-select     ; visually switch windows

         :emacs
         (ibuffer +icons)
         vc                ; remember, remember that commit in November
         (dired +icons) ; making dired pretty [functional]
         electric          ; smarter, keyword-based electric-indent

         :term
         eshell            ; a consistent, cross-platform shell (WIP)

         :tools
         docker
         eval
         (lookup +dictionary +docsets)
         proselint
         lsp
         ,@(when IS-MAC
             '(macos))
         make              ; run make tasks from Emacs
         editorconfig
         ein
         magit
         gist              ; interacting with github gists
         pass              ; password manager for nerds
         pdf               ; pdf enhancements
         rgb
         tmux            ; an API for interacting with tmux
         upload            ; map local to remote projects via ssh/ftp

         :checkers
         spell
         (syntax +childframe)

         :lang
         p4
         common-lisp
         jade
         sourcepawn
         ebnf
         ;;racket
         geiser
         assembly          ; assembly for fun or debugging
         (cc +lsp)                ; C/C++/Obj-C madness
         ;;crystal         ; ruby at the speed of c
         ;;clojure         ; java with a lisp
         ;;csharp          ; unity, .NET, and mono shenanigans
         data              ; config/data formats
         elixir            ; erlang done right
         elm             ; care for a cup of TEA?
         emacs-lisp        ; drown in parentheses
         ;;ess             ; emacs speaks statistics
         ;;go              ; the hipster dialect
         ;;(haskell +intero) ; a language that's lazier than I am
         (haskell +lsp)
         hy                ; readability of scheme w/ speed of python
         (java +lsp) ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
         (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
         ;;julia             ; a better, faster MATLAB
         (latex +pdf-tools) ; writing papers in Emacs has never been so fun
         ledger          ; an accounting system in Emacs
         lua             ; one-based indices? one-based indices
         markdown          ; writing docs for people to ignore
                                        ;ocaml             ; an objective camel
         (org              ; organize your plain life in plain text
          +gnuplot
          +dragndrop
          +pandoc
          +present)
         ;;perl              ; write code no one else can comprehend
         php               ; perl's insecure younger brother
         plantuml          ; diagrams for confusing people more
         ;;purescript        ; javascript, but functional
         (python +lsp)            ; beautiful is better than ugly
         rest              ; Emacs as a REST client
         ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
         (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
         ;;scala             ; java, but good
         (sh +fish)                ; she sells (ba|z)sh shells on the C xor
         ;;swift             ; who asked for emoji variables?
         web               ; the tubes

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
