(def-package! lsp-mode
  :commands (lsp-mode)
  :config
  (setq lsp-enable-xref t
        lsp-enable-indentation t
        lsp-enable-eldoc t
        lsp-enable-completion-at-point t))

(def-package! lsp-ui
  :init
  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 5)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-enable-completion-at-point t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header t
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-show-flycheck t
        lsp-ui-sideline-show-hover t
        lsp-ui-doc-border (doom-color 'fg))
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (with-eval-after-load "lsp-mode"
    (setq lsp-ui-flycheck-enable t))
  ;; (map! :map lsp-ui-mode-map
  ;;      [remap xref-find-definitions] . lsp-ui-peek-find-definitions
  ;;      [remap xref-find-references] . lsp-ui-peek-find-references)
  :hook
  (lsp-mode . lsp-ui-mode))


(def-package! company-lsp
  :after lsp-mode
  :config
  (set! :company-backend lsp-mode 'company-lsp)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t))


(def-package! lsp-python
  :commands (lsp-python-enable)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (set! :company-backend '(python-mode) '(company-lsp company-yasnippet))
  (set! :lookup 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  :hook
  (python-mode . lsp-python-enable))


(def-package! lsp-haskell
  :commands (lsp-haskell-enable)
  :config
  (set! :company-backend '(haskell-mode) '(company-lsp company-yasnippet))
  (set! :lookup 'haskell-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  :hook
  (haskell-mode . lsp-haskell-enable))


(def-package! lsp-rust
  :commands (lsp-rust-enable)
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (set! :company-backend '(rust-mode) '(company-lsp company-yasnippet))
  (set! :lookup 'rust-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  :hook
  (rust-mode . lsp-rust-enable))
