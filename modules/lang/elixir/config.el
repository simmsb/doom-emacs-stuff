;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))

(use-package! elixir-mode
  :defer t
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)

  :config
  (when (modulep! +lsp)
    (add-hook 'elixir-mode-local-vars-hook #'lsp!)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")))

  ;; ...and only complete the basics
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))

  (when (modulep! +lsp)
    (add-hook 'elixir-mode-local-vars-hook #'lsp! 'append)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")))

  (when (modulep! +tree-sitter)
    (add-hook 'elixir-mode-local-vars-hook #'tree-sitter! 'append))

  (after! highlight-numbers
    (puthash 'elixir-mode
             "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
             highlight-numbers-modelist)))

;; (after! web-mode
;;   (add-to-list 'web-mode-engines-alist '("elixir" . "\\.leex\\'")))
;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("nextls" "--stdio"))
;;           :multi-root t
;;           ;; :initialization-options '(:experimental (:completions (:enable t))) ;; Enable the experimental completion mode
;;           :activation-fn (lsp-activate-on "elixir")
;;           :server-id 'next-ls))

;;   (lsp-defcustom lsp-next-ls-completion-enabled t
;;     "Completions enabled"
;;     :type 'boolean
;;     :group 'next-ls
;;     :lsp-path "next-ls.nextLS.experimental.completions.enable")

;;   (lsp-defcustom lsp-next-ls-spitfire-enabled t
;;     "Spitfire enabled"
;;     :type 'boolean
;;     :group 'next-ls
;;     :lsp-path "next-ls.nextLS.spitfire"))
