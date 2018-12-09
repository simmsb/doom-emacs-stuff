(package! lsp-mode)
(package! company-lsp)
(package! lsp-ui)

(when (featurep! +rust)
  ;; doom-local-dir would be better but idk how to eval exprs in the recipe
  (package! lsp-rust :recipe (:fetcher github :repo "nitros12/lsp-rust"))
  (package! racer :disable t)
  (package! company-racer :disable t)
  (package! flycheck-rust :disable t))

(when (featurep! +python)
  ;; (package! lsp-python)
  (package! company-anaconda :disable t)
  (package! anaconda-mode :disable t))

(when (featurep! +haskell)
  (package! lsp-haskell))

(when (featurep! +cc)
  (package! ccls))

;; (when (featurep! +js)
;;   (package! lsp-javascript-typescript))
