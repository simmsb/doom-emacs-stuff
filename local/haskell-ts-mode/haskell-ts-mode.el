;;; haskell-ts-mode.el --- A treesit based major mode for haskell -*- lexical-binding:t -*-

;; Copyright (C) 2024  Pranshu Sharma


;; Author: Pranshu Sharma <pranshusharma366 at gmail>
;; URL: https://codeberg.org/pranshu/haskell-ts-mode
;; Package-Requires: ((emacs "29.3"))
;; Version: 1
;; Keywords: languages, haskell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode that uses treesitter to provide all the basic
;; major mode stuff, like indentation, font lock, etc...
;; It uses the grammer at: https://github.com/tree-sitter/tree-sitter-haskell

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

(defgroup haskell-ts-mode nil
  "Group that contains haskell-ts-mode variables"
  :group 'langs)

(defvar haskell-ts-font-lock-feature-list
  `((comment str pragma parens)
    (type definition function args)
    (match keyword)
    (otherwise signature type-sig)))

(defcustom haskell-ts-ghci "ghci"
  "The command to be called to run ghci."
  :type 'string)

(defcustom haskell-ts-ghci-buffer-name "Inferior Haskell"
  "Buffer name for the ghci prcoess."
  :type 'string)

(defcustom haskell-ts-use-indent t
  "Set to non-nil to use the indentation provided by haskell-ts-mode"
  :type 'boolean)

(defcustom haskell-ts-font-lock-level 4
  "Level of font lock, 1 for minimum highlghting and 4 for maximum."
  :type '(choice (const :tag "Minimal Highlighting" 1)
          (const :tag "Low Highlighting" 2)
          (const :tag "High Highlighting" 3)
          (const :tag "Maximum Highlighting" 4)))

(defvar haskell-ts-prettify-symbols-alist
  '(("\\" . "λ")
    ("/=" . "≠")
    ("->" . "→")
    ("=>" . "⇒")
    ("<-" . "←")
    ("<=" . "≥")
    (">=" . "≤")))

(defvar haskell-ts-font-lock
  (treesit-font-lock-rules
   :language 'haskell
   :feature 'keyword
   `(["module" "import" "data" "let" "where" "case" "type"
      "if" "then" "else" "of" "do" "in" "instance" "class"
      "newtype" "family" "deriving" "via" "stock" "anyclass"]
     @font-lock-keyword-face)
   :language 'haskell
   :feature 'otherwise
   :override t
   `(((match (guards guard: (boolean (variable) @font-lock-keyword-face)))
      (:match "otherwise" @font-lock-keyword-face)))
   :language 'haskell
   :feature 'type-sig
   "(signature (binding_list (variable) @font-lock-doc-markup-face))
    (signature (variable) @font-lock-doc-markup-face)"
   :language 'haskell
   :feature 'args
   :override 'keep
   (concat
    "(function (infix left_operand: (_) @haskell-ts--fontify-arg))"
    "(function (infix right_operand: (_) @haskell-ts--fontify-arg))"
    "(generator . (_) @haskell-ts--fontify-arg)"
    "(bind (as (variable) . (_) @haskell-ts--fontify-arg))"
    "(patterns) @haskell-ts--fontify-arg")
   :language 'haskell
   :feature 'type
   `((type) @font-lock-type-face
     (constructor) @font-lock-type-face)
   :language 'haskell
   :override t
   :feature 'signature
   `((signature (function) @haskell-ts--fontify-type)
     (context (function) @haskell-ts--fontify-type))
   :language 'haskell
   :feature 'match
   `((match ("|" @font-lock-doc-face) ("=" @font-lock-doc-face))
     (list_comprehension ("|" @font-lock-doc-face
                          (qualifiers (generator "<-" @font-lock-doc-face))))
     (match ("->" @font-lock-doc-face)))
   :language 'haskell
   :feature 'comment
   `(((comment) @font-lock-comment-face)
     ((haddock) @font-lock-doc-face))
   :language 'haskell
   :feature 'pragma
   `((pragma) @font-lock-preprocessor-face
     (cpp) @font-lock-preprocessor-face)
   :language 'haskell
   :feature 'str
   :override t
   `((char) @font-lock-string-face
     (string) @font-lock-string-face
     (quasiquote (quoter) @font-lock-type-face)
     (quasiquote (quasiquote_body) @font-lock-preprocessor-face))
   :language 'haskell
   :feature 'parens
   :override t
   `(["(" ")" "[" "]"] @font-lock-operator-face
     (infix operator: (_) @font-lock-operator-face))
   :language 'haskell
   :feature 'function
   :override t
   `((function name: (variable) @font-lock-function-name-face)
     (function (infix (operator)  @font-lock-function-name-face))
     (declarations (type_synomym (name) @font-lock-function-name-face))
     (bind (variable) @font-lock-function-name-face)
     (bind (infix (variable) @font-lock-function-name-face) "<-")
     (function (infix (infix_id (variable) @font-lock-function-name-face)))
     (bind (as (variable) @font-lock-function-name-face)))
    :language 'haskell
    :feature 'lambda
    :override t
    `((lambda ("\\" @font-lock-doc-face) ("->" @font-lock-doc-face))))
  "The treesitter font lock settings for haskell.")

(defun haskell-ts--standalone-parent (_ parent bol)
  (save-excursion
    (goto-char (treesit-node-start parent))
    (let ((type (treesit-node-type parent)))
      (if (and (not bol)
               (or (looking-back "^[ \t]*" (line-beginning-position))
                   (member
                    type
                    '("when" "where" "do" "let" "local_binds" "function"))))
          (treesit-node-start parent)
        (haskell-ts--standalone-parent 1 (funcall
                                          (if bol #'treesit-node-parent #'identity)
                                          (treesit-node-parent parent))
                                        nil)))))

(defvar haskell-ts--ignore-types
  (regexp-opt '("comment" "cpp" "haddock"))
  "Node types that will be ignored by indentation.")

(defun haskell-ts--p-sib (node &optional arg)
  (let* ((func (if arg
                   #'treesit-node-prev-sibling
                 #'treesit-node-next-sibling))
         (n (funcall func node)))
    (while (and n (string-match haskell-ts--ignore-types
                                (treesit-node-type n)))
      (setq n (funcall func n)))
    n))

(defun haskell-ts--p-prev-sib (node &optional parent bol)
  (treesit-node-start (haskell-ts--p-sib node t)))

(defun haskell-ts--p-n-prev (node)
  (haskell-ts--p-sib node t))

(defun haskell-ts--parent-first-child (_ parent _)
  (treesit-node-start (treesit-node-child parent 0)))

(defun haskell-ts--indent-for-decl (_ _ bol)
  (save-excursion
    (goto-char bol)
    (forward-line -1)
    (back-to-indentation)
    (let* ((node-on-line-above (treesit-node-parent (treesit-node-at (point))))
           (line-empty (looking-at-p "[[:blank:]]*$")))
      (if (not line-empty)
          (treesit-node-start node-on-line-above)
        nil))))

(defun haskell-ts--indent-for-decl-indent (_ _ bol)
  (save-excursion
    (goto-char bol)
    (forward-line -1)
    (back-to-indentation)
    (let* ((node-on-line-above (treesit-node-parent (treesit-node-at (point))))
           (type (treesit-node-type node-on-line-above))
           (line-empty (looking-at-p "[[:blank:]]*$")))
      (if (and (not line-empty)
               (member type '("class" "instance" "type_family")))
          2
        0))))

;; find anchor for a bind/let, expects to start on the rhs of the line above the
;; indent
(defun haskell-ts--find-bind-anchor (node)
  (let* ((parent (treesit-node-parent node))
         (parent-type (treesit-node-type parent))
         (node-type (treesit-node-type node)))
    (cond
     ((not node) nil)
     ;; at a bind, this can either be `a = b' or `a <- b'
     ;; for a `<-' the children are (var <- expr)
     ;; for a `=' the children are (var match)
     ;; this is quite annoying...
     ((string= parent-type "bind")
      (let* ((children (treesit-node-children parent)))
        ;; if the length is 3 or two, then the RHS probably exists, we can use that.
        ;; however if the length is greater than, return the bind variable so that we
        ;; indent to it
        (if (length< children 4)
          (let* ((rhs (car-safe (last children)))
                 ;; if the rhs is a match node this was an `a = b' bind
                 (real-rhs (if (string= (treesit-node-type rhs) "match")
                               (treesit-node-child rhs -1)
                             rhs)))
            (if (equal (treesit-node-start real-rhs)
                       (treesit-node-end real-rhs))
                ;; if the expr has zero size, return the bind instead. treesitter likes
                ;; to give a zero length variable for an unterminated `x <-'
              parent
              real-rhs))
          (car-safe children))))
     ;; on a let, find the local_binds and return the first
     ((string= node-type "let")
      (let* (;; is there a better way to distinguish between "let" and let nodes
             ;; (the literal and the expr)
             (true-let (if (string= parent-type "let")
                           parent
                         node))
             (local_binds (treesit-node-child true-let -1))
             (bind (treesit-node-child local_binds 0))
             (match (treesit-node-child bind -1))
             (expr (treesit-node-child match -1)))
        ;; if the expr has zero size, return the let instead. treesitter likes
        ;; to give a zero length variable for an unterminated `let ='
        (if (equal (treesit-node-start expr)
                   (treesit-node-end expr))
            bind
            expr)))
     ;; on a local_binds (can happen on multiline lets)
     ((string= parent-type "local_binds")
      (let* ((bind (car-safe (treesit-node-children parent)))
             (expr (car-safe (last (treesit-node-children bind)))))
        expr))
     ((string= parent-type "alternative")
      parent)
     ;; found a do, just return the child
     ((string= parent-type "do") node)
     (t (haskell-ts--find-bind-anchor parent)))))

;; fallback system which we use to indent something to an expression on a line
;; above, which might be part of a let/bind
(defun haskell-ts--indent-for-maybe-expr (_ _ bol)
  (save-excursion
    (let* ((node-on-line-above (progn
                                 (goto-char bol)
                                 (forward-line -1)
                                 (back-to-indentation)
                                 (treesit-node-at (point))))
           (node-on-end-line-above (progn
                                     (goto-char bol)
                                     (forward-line -1)
                                     (end-of-line)
                                     (treesit-node-at (point))))
           (this-line-empty (progn
                              (goto-char bol)
                              (back-to-indentation)
                              (looking-at-p "[[:blank:]]*$")))
           (line-above-empty (progn
                               (goto-char bol)
                               (forward-line -1)
                               (back-to-indentation)
                               (looking-at-p "[[:blank:]]*$"))))
      (when (and (not this-line-empty)
                 (not line-above-empty))
        (if (member (treesit-node-type (treesit-node-parent node-on-end-line-above))
                    '("signature" "ERROR"))
            (treesit-node-start node-on-line-above)
          (let ((node-below-bind (haskell-ts--find-bind-anchor node-on-line-above)))
            (treesit-node-start node-below-bind)))))))

(defun haskell-ts--grandparent-is (grand-parent-t)
  (lambda (_node parent &rest _)
    (let ((gp (treesit-node-parent parent)))
      (when (not (null gp))
        (string-match-p grand-parent-t (treesit-node-type gp))))))

(defvar haskell-ts--record-stuff
  (treesit-query-compile 'haskell '((record (constructor) @ctor :anchor "{" @bracket))))

;; handle within a record, where we want to indent to the opening brackrt if
;; it's on a new line
(defun haskell-ts--handle-record (_ parent _)
    (save-excursion
      (pcase (treesit-query-capture parent haskell-ts--record-stuff)
        (`((ctor . ,ctor) .
           ((bracket . ,bracket) . ,_))
         (let* ((ctor-line (line-number-at-pos (treesit-node-start ctor)))
                (bracket-line (line-number-at-pos (treesit-node-start bracket))))
           (if (equal ctor-line bracket-line)
               (treesit-node-start ctor)
             (treesit-node-start bracket)))))))

(defun haskell-ts--handle-tuple/list (node parent _)
  (let ((paren (treesit-node-child parent 0))
        (first-expr (treesit-node-child-by-field-name parent "element")))
    (cond ((equal (treesit-node-start node)
                  (treesit-node-start first-expr))
           (treesit-node-start paren))
          ((string= "," (treesit-node-type node))
           (treesit-node-start paren))
          (t (treesit-node-start first-expr)))))

(defvar haskell-ts-indent-rules
  `((haskell
     ((node-is "^cpp$") column-0 0)
     ((parent-is "^imports$") column-0 0)
     ;; Lambda
     ((parent-is "^lambda\\(_case\\)?$") standalone-parent 2)

     ((parent-is "^class_declarations$") standalone-parent 2)

     ((node-is "^where$") parent 2)

     ;; in
     ((node-is "^in$") parent 0)

     ((parent-is "qualifiers") parent 0)

     ;; list
     ((node-is "^]$") parent 0)

     ;; If then else
     ((node-is "^then$") parent 2)
     ((node-is "^else$") parent 2)

     ((parent-is "^infix$") parent 2)
     ((parent-is "^tuple\\|list$") haskell-ts--handle-tuple/list 0)

     ;; ((parent-is "^apply$") haskell-ts--indent-for-maybe-expr 2)

     ((node-is "^quasiquote$") grand-parent 2)
     ((parent-is "^quasiquote_body$") (lambda (_ _ c) c) 0)
     ((parent-is "^do$") standalone-parent 2)

     ((parent-is "^alternatives$") haskell-ts--p-prev-sib 0)

     ((parent-is "^data_constructors$") parent 0)

     ;; where
     ((lambda (node _ _)
        (let ((n (treesit-node-prev-sibling node)))
          (while (string= "comment" (treesit-node-type n))
            (setq n (treesit-node-prev-sibling n)))
          (string= "where" (treesit-node-type n))))

      (lambda (_ b _)
        (+ 1 (treesit-node-start (treesit-node-prev-sibling b))))
      3)
     ((parent-is "local_binds\\|instance_declarations") haskell-ts--p-prev-sib 0)

     ;; Match
     ((lambda (node _ _)
        (and (string= "match" (treesit-node-type node))
             (string-match (regexp-opt '("patterns" "variable"))
                           (treesit-node-type (haskell-ts--n-prev node)))))
      standalone-parent 2)

     ((node-is "match") haskell-ts--p-prev-sib 0)
     ((parent-is "match") standalone-parent 2)

     ((parent-is "^haskell$") column-0 0)
     ;; ((parent-is "^declarations$"))

     ((parent-is "^signature$") stand-alone-parent 2)

     ((parent-is "^record$") haskell-ts--handle-record 2)
     ((parent-is "^field_update$") parent 2)

     ((parent-is "^exports$")
      (lambda (_ b _) (treesit-node-start (treesit-node-prev-sibling b)))
      0)
     ((n-p-gp nil "signature" "foreign_import") grand-parent 3)
     ((parent-is "^case$") standalone-parent 2)
     ((node-is "^alternatives$")
      (lambda (_ b _)
        (treesit-node-start (treesit-node-child b 0)))
      2)
     ((node-is "^comment$")
      (lambda (node parent _)
        (pcase node
          ;; (relevent means type not it haskell-ts--ignore-types)
          ;; 1. next relevent sibling if exists
          ((haskell-ts--p-sib (and (pred (not null)) n))
           (treesit-node-start n))
          ;; 2. previous relevent sibling if exists
          ((haskell-ts--p-prev-sib (and (pred (not null)) n))
           n)
          ;; 3. parent
          (_ (treesit-node-start parent))))
      0)

     ((parent-is "^comment$") prev-adaptive-prefix 0)
     ((parent-is "^haddock$") column-0 0)

     ;; prev-adaptive-prefix is broken sometimes
     (no-node
      haskell-ts--indent-for-decl haskell-ts--indent-for-decl-indent)


     ;; Backup
     (catch-all haskell-ts--indent-for-maybe-expr 2)))
  "\"Simple\" treesit indentation rules for haskell.")

(defvar haskell-ts-mode-syntax-table
  (eval-when-compile
    (let ((table (make-syntax-table))
          (syntax-list
           `((" " ?\  ?\t)
             ("\"" ?\")
             ("_" ?\' ?_)
             ("()" ?\()
             (")(" ?\))
             ("(]" ?\[)
             (")[" ?\])
             ("(}1nb" ?\{)
             ("){4nb" ?\})
             ("< 123" ?-)
             ("<" ?\n)
             ("$`" ?\`)
             ,(cons
               "."
               (string-to-list "!#$%&*+./:<=>?@^|~,;\\")))))
      ;; The defaults are mostly fine
      (dolist (ls syntax-list table)
        (dolist (char (cdr ls))
          (modify-syntax-entry char (car ls) table))))))

(defun haskell-ts-sexp (node)
  "Returns non-nil on a sexp node."
  (let ((node-text (treesit-node-text node 1)))
    (and
     (not (member node-text '( "{" "}" "[" "]" "(" ")" ";")))
     (not (and (string= "operator" (treesit-node-field-name node))
               (= 1 (length node-text)))))))

(defvar haskell-ts-thing-settings
  `((haskell
     (sexp haskell-ts-sexp)
     (sentence "match")
     (string "string")
     (text "string")))
  "`treesit-thing-settings' for `haskell-ts-mode'.")

(defmacro haskell-ts-imenu-name-function (check-func)
  `(lambda (node)
     (let ((nn (treesit-node-child node 0 node)))
       (if (funcall ,check-func node)
           (if (string= (treesit-node-type nn) "infix")
               (treesit-node-text (treesit-node-child nn 1))
             (haskell-ts-defun-name node))
         nil))))

(defvar-keymap  haskell-ts-mode-map
  :doc "Keymap for haskell-ts-mode."
  "C-c C-c" #'haskell-ts-compile-region-and-go
  "C-c C-r" #'run-haskell)

;;;###autoload
(define-derived-mode haskell-ts-mode prog-mode "haskell ts mode"
  "Major mode for Haskell files using tree-sitter."
  :table haskell-ts-mode-syntax-table
  (unless (treesit-ready-p 'haskell)
    (error "Tree-sitter for Haskell is not available"))
  (setq treesit-primary-parser (treesit-parser-create 'haskell))
  (setq treesit-language-at-point-function
        (lambda (&rest _) 'haskell))
  (setq-local treesit-defun-type-regexp "\\(?:\\(?:function\\|struct\\)_definition\\)")
  ;; Indent
  (when haskell-ts-use-indent
    (setq-local treesit-simple-indent-rules haskell-ts-indent-rules)
    (setq-local indent-tabs-mode nil))
  ;; Comment
  (setq-local comment-start "-- ")
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip "\\(?: \\|^\\)-+")
  ;; Electric
  (setq-local electric-pair-pairs
              '((?` . ?`) (?\( . ?\)) (?{ . ?}) (?\" . ?\") (?\[ . ?\])))
  ;; Navigation
  (setq-local treesit-defun-name-function 'haskell-ts-defun-name)
  (setq-local treesit-thing-settings haskell-ts-thing-settings)
  (setq-local treesit-defun-type-regexp
              ;; Since haskell is strict functional, any 2nd level
              ;; entity is defintion
              (cons ".+"
                    (lambda (node)
                      (and (not (string-match haskell-ts--ignore-types (treesit-node-type node)))
                           (string= "declarations" (treesit-node-type (treesit-node-parent node)))))))
  (setq-local prettify-symbols-alist haskell-ts-prettify-symbols-alist)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              `((nil haskell-ts-imenu-func-node-p nil
                 ,(haskell-ts-imenu-name-function #'haskell-ts-imenu-func-node-p))
                ("Signatures.." haskell-ts-imenu-sig-node-p nil
                 ,(haskell-ts-imenu-name-function #'haskell-ts-imenu-sig-node-p))
                ("Data..." haskell-ts-imenu-data-type-p nil
                 (lambda (node)
                   (treesit-node-text (treesit-node-child node 1))))))
  ;; font-lock
  (setq-local treesit-font-lock-level haskell-ts-font-lock-level)
  (setq-local treesit-font-lock-settings haskell-ts-font-lock)
  (setq-local treesit-font-lock-feature-list
              haskell-ts-font-lock-feature-list)
  (treesit-major-mode-setup))

(defun haskell-ts--fontify-arg (node &optional _ _ _)
  (if (string= "variable" (treesit-node-type node))
      (put-text-property
       (treesit-node-start node)
       (treesit-node-end node)
       'face 'font-lock-variable-name-face)
    (mapc 'haskell-ts--fontify-arg (treesit-node-children node))))

(defun haskell-ts--fontify-type (node &optional _ _ _)
  (let ((last-child (treesit-node-child node -1)))
    (if (string= (treesit-node-type last-child) "function")
        (haskell-ts--fontify-type last-child)
      (put-text-property
       (treesit-node-start last-child)
       (treesit-node-end last-child)
       'face 'font-lock-variable-name-face))))

(defun haskell-ts-imenu-node-p (regex node)
  (and (string-match-p regex (treesit-node-type node))
       (string= (treesit-node-type (treesit-node-parent node)) "declarations")))

(defun haskell-ts-imenu-func-node-p (node)
  (haskell-ts-imenu-node-p "function\\|bind" node))

(defun haskell-ts-imenu-sig-node-p (node)
  (haskell-ts-imenu-node-p "signature" node))

(defun haskell-ts-imenu-data-type-p (node)
  (haskell-ts-imenu-node-p "data_type" node))

(defun haskell-ts-defun-name (node)
  (treesit-node-text (treesit-node-child node 0)))

(defun haskell-ts-compile-region-and-go (start end)
  "Compile the text from START to END in the haskell proc."
  (interactive "r")
  (let ((hs (haskell-ts-haskell-session))
        (str (buffer-substring-no-properties
              start end)))
    (comint-send-string hs ":{\n")
    (comint-send-string
     hs
     (replace-regexp-in-string "^:\\}" "\\:}" str nil t))
    (comint-send-string hs "\n:}\n")))

(defun run-haskell ()
  "Run an inferior Haskell process."
  (interactive)
  (let ((buffer (concat "*" haskell-ts-ghci-buffer-name "*")))
    (pop-to-buffer-same-window
     (if (comint-check-proc buffer)
         buffer
       (make-comint haskell-ts-ghci-buffer-name haskell-ts-ghci nil buffer-file-name)))))

(defun haskell-ts-haskell-session ()
  (get-buffer-process (concat "*" haskell-ts-ghci-buffer-name "*")))

(when (treesit-ready-p 'haskell)
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode)))

(provide 'haskell-ts-mode)

;;; haskell-ts-mode.el ends here
