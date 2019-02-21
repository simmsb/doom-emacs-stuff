(def-package! lsp-mode
  :commands (lsp)
  :config
  (add-hook! lsp-after-open 'lsp-enable-imenu)
  (setq lsp-enable-xref t
        lsp-prefer-flymake nil
        lsp-enable-indentation t
        lsp-enable-completion-at-point nil))

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
                  (internal-border-width . 0)
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
        lsp-ui-doc-position 'top
        lsp-ui-doc-header t
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-border (doom-color 'fg))
  ;; (map! :map lsp-ui-mode-map
  ;;      [remap xref-find-definitions] . lsp-ui-peek-find-definitions
  ;;      [remap xref-find-references] . lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(def-package! lsp-ui-flycheck
  :commands (lsp-ui-flycheck-enable)
  :after lsp-ui
  :init
  (lsp-ui-flycheck-enable t))

(def-package! company-lsp
  :after lsp-mode
  :config
  ;;(set! :company-backend lsp-mode '(company-lsp company-capf))
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t))

(when (featurep! +java)
  (require 'lsp-java)
  (def-package! lsp-java
    :config
    (set-formatter! 'java-mode #'lsp-format-buffer)
    (set-company-backend! 'java-mode 'company-lsp)
    (set-lookup-handlers! 'java-mode
      :definition #'lsp-ui-peek-find-definitions
      :references #'lsp-ui-peek-find-references)
    :hook
    (java-mode . lsp)))

(when (featurep! +python) ; builtin
  (add-hook! python-mode #'lsp)
  (set-formatter! 'python-mode #'lsp-format-buffer)
  (set-company-backend! 'python-mode 'company-lsp)
  (set-lookup-handlers! 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

(when (featurep! +haskell)
  (def-package! lsp-haskell
    :after lsp-mode
    :config
    (setq lsp-enable-snippet nil)
    (set-formatter! 'haskell-mode #'lsp-format-buffer)
    (set-company-backend! 'haskell-mode 'company-lsp)
    (set-lookup-handlers! 'haskell-mode
      :definition #'lsp-ui-peek-find-definitions
      :references #'lsp-ui-peek-find-references)
    (add-hook! haskell-mode #'lsp)))

(when (featurep! +rust)
  (def-package! lsp-rust
    :after lsp-mode
    :config
    (add-hook! rust-mode #'lsp)
    (set-formatter! 'rust-mode #'lsp-format-buffer)
    (set-company-backend! 'rust-mode 'company-lsp)
    (set-lookup-handlers! 'rust-mode
      :definition #'lsp-ui-peek-find-definitions
      :references #'lsp-ui-peek-find-references)
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    (add-hook! rust-mode #'lsp)))

(when (featurep! +js)
  (add-hook! '(js-mode js3-mode rjsx-mode) #'lsp)
  (set-formatter! 'js-mode #'lsp-format-buffer)
  (set-company-backend! '(js-mode js3-mode rjsx-mode) 'company-lsp)
  (set-lookup-handlers! '(js-mode js3-mode rjsx-mode)
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)

  ;; (def-package! lsp-javascript-typescript
  ;;   :config
  ;;   :hook
  ;;   ((js-mode js3-mode rjsx-mode) . lsp))

  ;; (defun lsp-javascript-typescript-company-transformer (candidates)
  ;;   (let ((completion-ignore-case t))
  ;;     (all-completions (company-grab-symbol) candidates)))

  ;; (defun lsp-javascript-typescript-js-hook ()
  ;;   (make-local-variable 'company-transformers)
  ;;   (push 'lsp-javascript-typescript-company-transformer company-transformers))

  ;; (add-hook! js-mode #'lsp-javascript-typescript-js-hook)
  )
