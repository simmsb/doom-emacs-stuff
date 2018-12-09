;;; lang/lsp-eglot/config.el -*- lexical-binding: t; -*-


(defun add-to-flycheck-list-or-set (val)
  "if flycheck-global-modes is a list, add to list, otherwise set to `val'"
  (setq flycheck-global-modes
        (cond
         ((not (listp flycheck-global-modes))
          val)
         ((eq (car flycheck-global-modes) 'not)
          (append val (cdr flycheck-global-modes)))
         (t
          (append val flycheck-global-modes)))))


(def-package! eglot
  :commands (eglot eglot-ensure)
  :init
  (when (featurep! +rust)
    (set-company-backend! 'rust-mode 'company-capf)
    (set-lookup-handlers! 'rust-mode
      :definition #'xref-find-definitions-other-window
      :references #'xref-find-references)
    (add-hook! rust-mode #'eglot-ensure)
    (after! flycheck
      (add-to-flycheck-list-or-set '(not rust-mode))))

  (when (featurep! +python)
    (set-company-backend! 'python-mode 'company-capf)
    (set-lookup-handlers! 'python-mode
      :definition #'xref-find-definitions-other-window
      :references #'xref-find-references)
    (add-hook! python-mode #'eglot-ensure)
    (after! flycheck
      (add-to-flycheck-list-or-set '(not python-mode))))

  (when (featurep! +haskell)
    (set-company-backend! 'haskell-mode 'company-capf)
    (set-lookup-handlers! 'haskell-mode
      :definition #'xref-find-definitions-other-window
      :references #'xref-find-references)
    (add-hook! haskell-mode #'eglot-ensure)
    (after! flycheck
      (add-to-flycheck-list-or-set '(not haskell-mode))))

  (when (featurep! +js)
    (set-company-backend! 'js-mode 'company-capf)
    (set-lookup-handlers! 'js-mode
      :definition #'xref-find-definitions-other-window
      :references #'xref-find-references)
    (add-hook! js-mode #'eglot-ensure)
    (after! flycheck
      (add-to-flycheck-list-or-set '(not js-mode))))

  (when (featurep! +cc)
    (set-company-backend! '(c-mode c++-mode) 'company-capf)
    (set-lookup-handlers! '(c-mode c++-mode)
      :definition #'xref-find-definitions-other-window
      :references #'xref-find-references)
    (add-hook! (c-mode c++-mode) #'eglot-ensure)
    (after! flycheck
      (add-to-flycheck-list-or-set '(not c-mode c++mode))))

  :config
  (when (featurep! +rust)
    (add-to-list 'eglot-server-programs '(rust-mode . (eglot-rls "rustup" "run" "nightly" "rls")))))

