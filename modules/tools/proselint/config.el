;;; feature/proselint/config.el -*- lexical-binding: t; -*-

(after! flycheck
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (flyspell-mode))

  (add-to-list 'flycheck-checkers 'proselint))
