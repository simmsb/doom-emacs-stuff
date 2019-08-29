;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(use-package! elixir-mode
  :defer t
  :config
  ;; ...and only complete the basics
  (set-company-backend! 'elixir-mode 'company-lsp))

(use-package! lsp-mode
 :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection #'(lambda () (f-join doom-private-dir "local" "elixir-ls" "build" "language_server.sh")))
                    :major-modes '(elixir-mode)
                    :priority 10
                    :server-id 'elixir-lsp)))


(after! elixir-mode
  (add-hook! elixir-mode #'lsp))
