;; -*- no-byte-compile: t; -*-
;;; init.el -*- lexical-binding: t; -*-

(doom! :feature
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       (syntax-checker
        +childframe)     ; tasing you for every semicolon you forget
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       format
       parinfer
       rotate-text       ; cycle region at point between text candidates

       :completion
       (company
        +auto)
       ;; +childframe)    ; the ultimate code completion backend
       (ivy +fuzzy)      ; a search engine for love and life
       ;;(helm +fuzzy)              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...

       :ui
       deft
       vc-gutter
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       (pretty-code      ; replace bits of code with pretty symbols
        +fira)
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       evil-goggles      ; display visual hints when editing in evil
       unicode           ; extended unicode support for various languages
       ;;neotree           ; a project drawer, like NERDTree for vim
       treemacs
       ;;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :emacs
       vc                ; remember, remember that commit in November
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ediff
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
       term              ; terminals in Emacs

       :tools
       ;;macos           ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit
       gist              ; interacting with github gists
       password-store    ; password manager for nerds
       pdf               ; pdf enhancements
       rgb
       ;;tmux            ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :lang
       jade
       sourcepawn
       (lsp +haskell +python +js +rust)
       ;;(lsp-eglot +haskell)
       ebnf
       ;; racket
       geiser
       assembly          ; assembly for fun or debugging
       (cc +irony +rtags)       ; C/C++/Obj-C madness
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
       haskell
       hy                ; readability of scheme w/ speed of python
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       (latex +pdf-tools) ; writing papers in Emacs has never been so fun
       ;;ledger          ; an accounting system in Emacs
       ;;lua             ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
                                        ;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present         ; Emacs for presentations
        +publish)        ; Emacs+Org as a static site generator
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
       ;;swift             ; who asked for emoji variables?
       ;;typescript        ; javascript, but better
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;(email +gmail)    ; emacs as an email client
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       (write            ; emacs as a word processor (latex + org + markdown)
        +wordnut         ; wordnet (wn) search
        +langtool)       ; a proofreader (grammar/style check) for Emacs
       :collab
       impatient-mode
       :config
       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +snippets +evil-commands))

(setq user-mail-address "ben@bensimms.moe"
      user-full-name "Ben Simms")

(setq company-box-enable-icon nil)
(setq custom-safe-themes t)

(add-hook! text-mode
  #'auto-save-mode)

(fringe-mode 8)
(setq doom-fringe-size 8)
(show-paren-mode t)
(setq show-paren-style 'mixed
      show-paren-delay 0)

(setq default-directory "~/dev/"
      default-tab-width 4)

(setq +rust-src-dir (concat (replace-regexp-in-string
                             "\n\\'" ""
                             (shell-command-to-string "rustc --print sysroot"))
                            "/lib/rustlib/src"))

(add-to-list 'exec-path "~/.local/bin/")
(add-to-list 'exec-path "~/.cargo/bin/")

(setq explicit-shell-file-name "/bin/bash"
      shell-file-name "/bin/bash")

(pcase (system-name)
  ("laptop"
   (toggle-frame-maximized)
   (setq doom-font (font-spec :family "Fira Mono" :size 14)
         doom-unicode-font (font-spec :family "Fira Mono")
         doom-big-font (font-spec :family "Fira Mono" :size 18)))
  (_
   (setq doom-font (font-spec :family "Fira Mono" :size 18)
         doom-unicode-font (font-spec :family "Fira Mono")
         doom-big-font (font-spec :family "Fira Mono" :size 23))))

;; we still need this apparently
(setq custom-file (concat doom-etc-dir "custom.el"))

(setq lsp-print-io t)
