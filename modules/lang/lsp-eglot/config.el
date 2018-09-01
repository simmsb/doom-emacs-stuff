;;; lang/lsp-eglot/config.el -*- lexical-binding: t; -*-

(def-package! eglot
  :commands (eglot eglot-ensure eglot-completion-at-point)
  :config
  (when (featurep! +rust)
    (set-company-backend! 'rust-mode '(eglot-completion-at-point)))
  (when (featurep! +python)
    (set-company-backend! 'python-mode '(eglot-completion-at-point))
    (add-hook! python-mode #'eglot-ensure))
  (when (featurep! +haskell)
    (set-company-backend! 'haskell-mode '(eglot-completion-at-point))
    (add-hook! haskell-mode #'eglot-ensure))
  (when (featurep! +js)
    (set-company-backend! 'js-mode '(eglot-completion-at-point))
    (add-hook! js-mode #'eglot-ensure))
  ;; use nightly rls
  (add-to-list 'eglot-server-programs '(rust-mode . (eglot-rls "rustup" "run" "nightly" "rls"))))


;;;###autoload
(defun add-to-list-or-set (maybe-list val)
  "if `maybe-list' is a list, add to list, otherwise set to '(val)"
  (if (listp (symbol-value maybe-list))
      (add-to-list maybe-list val)
    (set maybe-list (list val))))

(when (featurep! +rust)
  (after! flycheck
    (add-to-list-or-set 'flycheck-global-modes '(not rust-mode)))
  (add-hook! rust-mode #'eglot-ensure))
