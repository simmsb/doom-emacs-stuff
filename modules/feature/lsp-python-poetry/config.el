;;; feature/lsp-python-poetry/config.el -*- lexical-binding: t; -*-

(require 'lsp-mode)
(require 'lsp-pyls)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("poetry" "run" "pyls"))
                  :major-modes '(python-mode)
                  :priority 2
                  :server-id 'pyls-poetry
                  :activation-fn (lambda (file-name major-mode)
                                   (and (eq 'python-mode major-mode)
                                        (eq 0 (call-process "poetry" nil nil nil "show"))))
                  :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration (lsp-configuration-section "pyls"))))))
