;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(require 'lsp-mode)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'(lambda () (f-join doom-private-dir "local" "elixir-ls" "build" "language_server.sh")))
                  :major-modes '(elixir-mode)
                  :priority 10
                  :server-id 'elixir-lsp))

(def-package! elixir-mode
  :defer t
  :config
  ;; ...and only complete the basics
  (set-company-backend! 'elixir-mode 'company-lsp))

(after! elixir-mode
  (add-hook! elixir-mode #'lsp))
