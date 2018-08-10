(package! lsp-mode)
(package! company-lsp)
(package! lsp-ui)

(when (featurep! +rust)
  (package! lsp-rust))

(when (featurep! +python)
  (package! lsp-python))

(when (featurep! +haskell)
  (package! lsp-haskell)
  (package! hindent))

(when (featurep! +cc)
  (package! ccls))

(when (featurep! +js)
  (package! lsp-javascript-typescript))
