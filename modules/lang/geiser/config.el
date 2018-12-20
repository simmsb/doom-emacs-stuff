;;; lang/geiser/config.el -*- lexical-binding: t; -*-

(def-package! geiser
  :commands geiser-mode geiser-company-backend
  :config
  (setq geiser-active-implementations '(racket)
        geiser-mode-start-repl-p t)

  (set-docsets! 'racket-mode "Racket")
  (set-docsets! 'scheme-mode "Scheme")

  (set-pretty-symbols! '(scheme-mode racket-mode)
    :lambda "lambda")

  (set-company-backend! 'geiser-mode 'geiser-company-backend)

  (set-repl-handler! '(scheme-mode racket-mode)
                     #'+geiser/repl)

  :hook
  ((scheme-mode racket-mode) . geiser-mode))

;; haha yes
(add-hook! geiser-mode
  (when (eq geiser-impl--implementation 'racket)
    (company-quickhelp-local-mode -1)))

(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))
