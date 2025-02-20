;;; numbat-ts-mode.el --- A treesit major mode for numbat -*- lexical-binding: t; -*-


;; Author: Ben Simms
;; Package-Requires: ((emacs "29.3"))
;; Version: 1
;; Keywords: languages, numbat

;;; Commentary:

;; This is a major mode that uses treesitter to provide all the basic
;; major mode stuff, like indentation, font lock, etc...

;;; Code:

(require 'comint)
(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defgroup numbat-ts-mode nil
  "Group that contains numbat-ts-mode variables"
  :group 'langs)

(defvar numbat-ts-font-lock-feature-list
  `((comment str constant)
    (keyword operator type)
    (parens function variable)
    (delimiter ifactor)))

(defcustom numbat-ts-font-lock-level 4
  "Level of font lock, 1 for minimum highlghting and 4 for maximum."
  :type '(choice (const :tag "Minimal Highlighting" 1)
          (const :tag "Low Highlighting" 2)
          (const :tag "High Highlighting" 3)
          (const :tag "Maximum Highlighting" 4)))

(defvar numbat-ts-font-lock
  (treesit-font-lock-rules
   :language 'numbat
   :feature 'keyword
   :override t
   '(["if" "then" "else" "use" "dimension" "unit" "let" "fn" "print" "assert_eq" "type" "where" "and" "struct"]
     @font-lock-keyword-face)

   :language 'numbat
   :feature 'constant
   '((number) @font-lock-number-face
     (boolean) @font-lock-constant-face)

   :language 'numbat
   :feature 'ifactor
   '((ifactor right: (_) @font-lock-builtin-face))

   :language 'numbat
   :feature 'variable
   '((variable_decl name: ((identifier) @font-lock-variable-name-face)))

   :language 'numbat
   :feature 'delimiter
   '(["::" ","] @font-lock-comment-delimiter-face)

   :language 'numbat
   :feature 'operator
   '([
      (multiply)
      (minus)
      (plus)
      (divide)
      (pow_symbol)
      (unicode_exponent)
      "//"
      "=="
      "!"
      "!="
      "≠"
      "<="
      "≤"
      "<"
      ">="
      "≥"
      ">"]
     @font-lock-operator-face)

   :language 'numbat
   :feature 'function
   '(
     (call name: ((identifier) @font-lock-function-call-face))

     (function_decl name: ((identifier) @font-lock-function-name-face)))


   :language 'numbat
   :feature 'type
   '(
     (type_annotation) @font-lock-type-face
     (dimension_expr) @font-lock-type-face)

   :language 'numbat
   :feature 'comment
   '((line_comment) @font-lock-comment-face)

   :language 'numbat
   :feature 'str
   :override t
   '(
     (string) @font-lock-string-face)

   :language 'numbat
   :feature 'parens
   '(["(" ")" "[" "]"] @font-lock-operator-face))

  "The treesitter font lock settings for numbat.")

(defvar numbat-ts-indent-rules
  `((numbat
     ((parent-is "^function_decl$") parent 2)
     (catch-all column-0 0)))
  "\"Simple\" treesit indentation rules for numbat.")

;;;###autoload
(define-derived-mode numbat-ts-mode prog-mode "Numbat"
  "Major mode for Numbat files using tree-sitter."
  (unless (treesit-ready-p 'numbat)
    (error "Tree-sitter for Numbat is not available"))
  (setq treesit-primary-parser (treesit-parser-create 'numbat))
  (setq treesit-language-at-point-function
        (lambda (&rest _) 'numbat))

  ;; Indent
  ;; (setq-local treesit-simple-indent-rules numbat-ts-indent-rules)
  (setq-local indent-tabs-mode nil)
  ;; Comment
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-auto-fill-only-comments t)

  (setq-local paragraph-start "\\s-*$")

  ;; Electric
  (setq-local electric-pair-pairs
              '((?` . ?`) (?\( . ?\)) (?{ . ?}) (?\" . ?\") (?\[ . ?\])))

  ;; font-lock
  (setq-local treesit-font-lock-level numbat-ts-font-lock-level)
  (setq-local treesit-font-lock-settings numbat-ts-font-lock)
  (setq-local treesit-font-lock-feature-list
              numbat-ts-font-lock-feature-list)
  (treesit-major-mode-setup))

(when (treesit-ready-p 'numbat)
  (add-to-list 'auto-mode-alist '("\\.nbt\\'" . numbat-ts-mode)))

(provide 'numbat-ts-mode)

;;; numbat-ts-mode.el ends here
