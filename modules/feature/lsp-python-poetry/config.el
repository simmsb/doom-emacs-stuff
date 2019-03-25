;;; feature/lsp-python-poetry/config.el -*- lexical-binding: t; -*-

(require 'lsp-mode)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () '("poetry" "run" "pyls")))
                  :major-modes '(python-mode)
                  :priority 2
                  :server-id 'pyls-poetry
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration `(:pyls ,lsp-clients-python-settings))))
                  :library-folders-fn (lambda (_workspace)
                                        lsp-clients-python-library-directories)))
