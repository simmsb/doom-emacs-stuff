;;; checkers/jinx/config.el -*- lexical-binding: t; -*-

(use-package! jinx
  :hook (text-mode . jinx-mode)
  :general ([remap ispell-word] #'jinx-correct)
  :init
      (defvar +spell-excluded-faces-alist
        '((markdown-mode
           . (markdown-code-face
              markdown-html-attr-name-face
              markdown-html-attr-value-face
              markdown-html-tag-name-face
              markdown-inline-code-face
              markdown-link-face
              markdown-markup-face
              markdown-plain-url-face
              markdown-reference-face
              markdown-url-face))
          (org-mode
           . (org-block
              org-block-begin-line
              org-block-end-line
              org-cite
              org-cite-key
              org-code
              org-date
              org-footnote
              org-formula
              org-inline-src-block
              org-latex-and-related
              org-link
              org-meta-line
              org-property-value
              org-ref-cite-face
              org-special-keyword
              org-tag
              org-todo
              org-todo-keyword-done
              org-todo-keyword-habt
              org-todo-keyword-kill
              org-todo-keyword-outd
              org-todo-keyword-todo
              org-todo-keyword-wait
              org-verbatim))
          (latex-mode
           . (font-latex-math-face
              font-latex-sedate-face
              font-lock-function-name-face
              font-lock-keyword-face
              font-lock-variable-name-face)))
        "Faces in certain major modes that spell-fu will not spellcheck.")
             )

(after! web-mode
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.leex\\'")))
