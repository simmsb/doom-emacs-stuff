(package! lsp-mode)
(package! company-lsp)
(package! lsp-ui)

(when (featurep! +rust)
  (package! lsp-rust)
  (package! company-racer :disable t)
  (package! flycheck-rust :disable t))

(when (featurep! +python)
  (package! lsp-python)
  (package! company-anaconda :disable t)
  (package! anaconda-mode :disable t))

(when (featurep! +haskell)
  (package! lsp-haskell))

(when (featurep! +cc)
  (package! ccls))

(when (featurep! +js)
  (package! lsp-javascript-typescript))
