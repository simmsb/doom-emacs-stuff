;;; private/ben/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)

(setq! warning-minimum-level :error)
(setq! compile-angel-predicate-function
        (lambda (file)
          (and (not (and (file-in-directory-p file doom-user-dir)
                         (not (file-in-directory-p file (expand-file-name "local" doom-user-dir)))))
               (not (file-in-directory-p file (expand-file-name "lisp" doom-emacs-dir)))
               (not (file-in-directory-p file (expand-file-name doom-modules-dir))))))

(compile-angel-on-load-mode)
(add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

(defun first-font (&rest fonts)
  (cl-find-if #'find-font fonts))

(pcase (system-name)
  ("home"
   (setq! doom-font (first-font
                     (font-spec :family "MonoLisa" :size 15)
                     (font-spec :family "Fira Code" :size 15))
           doom-big-font (first-font
                          (font-spec :family "MonoLisa" :size 22)
                          (font-spec :family "Fira Code" :size 22))
           doom-variable-pitch-font (font-spec :family "Fira Sans")
           doom-serif-font (font-spec :family "Fira Code" :size 16)))
  ("laptop2"
   (setq! doom-font (first-font
                     (font-spec :family "MonoLisa" :size 14)
                     (font-spec :family "Fira Code" :size 14))
           doom-big-font (first-font
                          (font-spec :family "Fira Code" :size 28))
           doom-serif-font (font-spec :family "Latin Modern Mono" :size 18)))
  ("worklaptop"
   (setq! doom-font (first-font
                     (font-spec :family "MonoLisa" :size 14)
                     (font-spec :family "Fira Code" :size 14))
           doom-big-font (first-font
                          (font-spec :family "Fira Code" :size 28))
           doom-serif-font (font-spec :family "Latin Modern Mono" :size 18))))


;; bindings
(map!
 (:leader
  (:prefix "f"
   :desc "Toggle Treemacs" "t" #'+treemacs/toggle-project)
  (:prefix "o"
   :desc "Open Shopping" "s" #'org-shopping-open
   :desc "Open kill ring" "k" #'+default/yank-pop
   :desc "Open notes.org" "n" #'org-notes-open)
  (:prefix "s"
   :desc "Search project (affe)" "P" #'affe-grep
   :desc "Lookup word in dict" "t" #'odict-lookup
   :desc "Fuzzy search word in dict" "T" #'odict-search)

  (:prefix "g"
   :desc "JJ or Magit log" "g" #'jj-or-magit-log)

  :desc "Vertico repeat select" "\"" #'vertico-repeat-select)

 (:map evilem-map
  :after evil-easymotion
  "<down>" #'evilem-motion-next-line
  "<up>" #'evilem-motion-previous-line)

 (:map evil-window-map
       "<left>"     #'evil-window-left
       "<right>"    #'evil-window-right
       "<up>"       #'evil-window-up
       "<down>"     #'evil-window-down)

 (:map evil-insert-state-map
       "C-<tab>" #'doom/dumb-indent
       "S-<tab>" #'doom/dumb-dedent)

 (:map evil-normal-state-map
       "q" nil
       "^" #'doom/backward-to-bol-or-indent
       "$" #'doom/forward-to-last-non-comment-or-eol)

 ;; (:map evil-normal-state-map
 ;;       "d" #'evil-delete-without-register-if-whitespace)
 ;; in lisp modes use default evil-delete for parinfer magic
 ;; (:mode (emacs-lisp-mode clojure-mode scheme-mode lisp-mode)
 ;;  :i "<backspace>" #'parinfer-backward-delete-char
 ;;  :i "C-d" #'delete-char)

 "<home>" #'back-to-indentation-or-beginning
 "<end>" #'end-of-line
 "s-q" #'prog-fill-reindent-defun
 "<f6>" #'evil-switch-to-windows-last-buffer
 "<wheel-left>" #'do-nothing
 "<wheel-right>" #'do-nothing
 "<double-wheel-left>" #'do-nothing
 "<double-wheel-right>" #'do-nothing
 "<triple-wheel-left>" #'do-nothing
 "<triple-wheel-right>" #'do-nothing)

(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq! input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))

(use-package! affe
  :after '(orderless)
  :custom
  ((affe-regexp-compiler . #'affe-orderless-regexp-compiler)))

(defun do-nothing (&rest _)
  (interactive)
  "Does nothing.")

(use-package! vc-jj)
(use-package! jj-mode
  :load-path "~/.doom.d/local/jj-mode.el"
  :config
  (setq! jj-log-display-function #'switch-to-buffer)
  (defconst evil-collection-jj-mode-maps '(jj-mode-map))
  (evil-set-initial-state 'jj-mode 'normal)
  (evil-collection-define-key 'normal 'jj-mode-map
    "n" 'magit-section-forward
    "p" 'magit-section-backward
    "." 'jj-goto-current
    "TAB" 'magit-section-toggle
    "q" 'quit-window

    ;; Basic operations
    "g" 'jj-log-refresh
    "c" 'jj-commit
    "e" 'jj-edit-changeset
    "u" 'jj-undo
    "N" 'jj-new-transient
    "s" 'jj-squash-transient
    "c" 'jj-commit
    "d" 'jj-describe
    "a" 'jj-abandon

    ;; Advanced Operations
    "RET" 'jj-enter-dwim
    "b" 'jj-bookmark-transient
    "r" 'jj-rebase-transient
    "G" 'jj-git-transient

    ;; Experimental
    "D" 'jj-diff
    "E" 'jj-diffedit-emacs
    "M" 'jj-diffedit-smerge

    "?" 'jj-mode-transient))


(use-package! disable-mouse)
(use-package! github-review)
(use-package! github-browse-file)

(use-package! zone-matrix-wake-up)

(use-package! dired)

(use-package! outline-minor-faces
  :defer t)

(use-package! geros
  :defer t
  :config
  (setq! geros-eval-result-duration nil))

(use-package! evil-lion
  :config
  (evil-lion-mode))

(use-package! sqlup-mode
  :defer t
  :commands (sqlup-mode)
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(use-package! backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package! esh-autosuggest
  :defer t
  :hook (eshell-mode . esh-autosuggest-mode))

;; (use-package! lsp-copilot
;;   :config
;;   (setq! lsp-copilot-user-languages-config (f-join doom-user-dir "languages.toml"))
;;   (add-hook! '(
;;                tsx-ts-mode-hook
;;                js-ts-mode-hook
;;                typescript-mode-hook
;;                typescript-ts-mode-hook
;;                rjsx-mode-hook
;;                less-css-mode-hook
;;                web-mode-hook
;;                python-ts-mode-hook
;;                rust-mode-hook
;;                rustic-mode-hook
;;                rust-ts-mode-hook
;;                toml-ts-mode-hook
;;                conf-toml-mode-hook
;;                bash-ts-mode-hook)
;;               #'lsp-copilot-mode))

;; Doom Emacs
(set-lookup-handlers! 'lsp-copilot-mode
  :definition '(lsp-copilot-find-definition :async t)
  :references '(lsp-copilot-find-references :async t)
  :implementations '(lsp-copilot-find-implementations :async t)
  :type-definition '(lsp-copilot-find-type-definition :async t)
  :documentation '(lsp-copilot-describe-thing-at-point :async t))

(after! lsp-haskell
  (setq! lsp-haskell-formatting-provider "ormolu"
         lsp-haskell--original-server-args lsp-haskell-server-args
         ;; lsp-haskell-server-args `(,@lsp-haskell-server-args "+RTS" "-N8" "-xn" "-RTS")
         lsp-haskell-server-args `(,@lsp-haskell--original-server-args "+RTS" "-N8" "-xn" "-RTS")
         ;; lsp-haskell-server-args `(,@lsp-haskell--original-server-args "+RTS" "-N8" "-c" "-H" "-RTS")
         lsp-haskell-plugin-ghcide-type-lenses-config-mode "always"
         lsp-haskell-tactics-on nil
         lsp-haskell-plugin-rename-config-cross-module t
         lsp-haskell-max-completions 100
         lsp--show-message nil
         lsp-haskell-session-loading "multipleComponents")
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed)

  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql lsp-haskell)))
    "Display the type signature of the function under point."
    (let* ((groups (--filter (s-equals? "```haskell" (car it))
                             (-partition-by #'s-blank?
                                            (->> (lsp-get contents :value)
                                                 s-trim
                                                 s-lines))))
           (type-sig-group
            (car (--filter (--any? (s-contains? (symbol-name (symbol-at-point))
                                                it)
                                   it)
                           groups))))
      (lsp--render-string
       (->> (or type-sig-group (car groups))
            (-drop 1)                     ; ``` LANG
            (-drop-last 1)                ; ```
            (-map #'s-trim)
            (s-join " "))
       "haskell"))))



;; no idea mate
;; (after! browse-url
;;   (defun browse-url (url)
;;     (browse-url-generic url)))

(after! nix-mode
  (setq! nix-nixfmt-bin "nixpkgs-fmt"))
;; (set-formatter! 'nixpkgs-fmt "nixpkgs-fmt" :modes 'nix-mode))

;; (defun config-brossa-lsp-server (workspace)
;;   (with-lsp-workspace workspace
;;     (lsp--set-configuration
;;      `(:brossa
;;        (:languageServer
;;         (:inlayHints
;;          (:cutoff 999999)))))))

;; (after! art-mode
;;   (require 'lsp-mode)
;;   (add-to-list 'lsp-language-id-configuration '(art-mode . "brossa"))
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection "brossa-lsp-server")
;;     :activation-fn (lsp-activate-on "brossa")
;;     :initialized-fn 'config-brossa-lsp-server
;;     :server-id 'brossa-lsp)))

(after! lsp-mode
  (setq! lsp-lens-enable nil
          +lsp-defer-shutdown nil
          lsp-inlay-hint-enable t
          lsp-modeline-diagnostics-scope :project
          lsp-restart 'auto-restart
          lsp-enable-indentation t
          lsp-enable-relative-indentation t
          lsp-enable-file-watchers t
          lsp-headerline-breadcrumb-enable nil
          lsp-ui-doc-show-with-cursor nil
          lsp-ui-sideline-show-hover nil
          lsp-ui-sideline-diagnostic-max-lines 10
          lsp-auto-execute-action nil)
  (dolist (dir '(
                 "[/\\\\]\\.venv\\'"
                 "[/\\\\]assets"
                 "[/\\\\]\\.embuild\\'"
                 "[/\\\\]result\\'"
                 "[/\\\\]_build\\'"
                 "[/\\\\]build\\'"
                 "[/\\\\]node_modules\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir))


  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(defvar known-parser-results (make-hash-table :test 'equal))

(defun my-treesit-language-available-p (orig &rest args)
  "Keep track of failures"
  (if (hash-table-contains-p args known-parser-results)
      (gethash args known-parser-results)
    (let* ((r (apply orig args)))
      (puthash args r known-parser-results)
      r)))

(advice-add 'treesit-language-available-p :around #'my-treesit-language-available-p)

(after! magit
  (setq! git-commit-summary-max-length 72)

  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)


  (defvar magit--rev-name-cache (make-hash-table :test 'equal))
  (defvar magit--rev-name-cache-revision nil)

  (defun magit--rev-name-cached (orig-fun rev &optional pattern not-anchored)
    (let* ((current-head (magit-rev-parse "HEAD")))
      (when (not (equal current-head magit--rev-name-cache-revision))
        (clrhash magit--rev-name-cache)
        (set-variable 'magit--rev-name-cache-revision current-head)))
    (let ((e (gethash rev magit--rev-name-cache)))
      (if e
          e
        (let ((e (apply orig-fun rev pattern not-anchored)))
          (puthash rev e magit--rev-name-cache)
          e))))

  (advice-add 'magit-rev-name :around #'magit--rev-name-cached))

(use-package! magit-prime
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))

(after! flycheck
  (add-hook! haskell-mode
    (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))
  (setq! flycheck-popup-tip-error-prefix "❌ "))

(after! flycheck-posframe
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
  (setq! flycheck-posframe-warning-prefix "⚠ "
          flycheck-posframe-error-prefix "❌ "
          flycheck-posframe-info-prefix "ⓘ "))

(after! rustic
  (setq! rustic-lsp-server 'rust-analyzer
          rustic-treesitter-derive t))

(after! lsp-rust
  (setq! lsp-rust-analyzer-display-chaining-hints nil
          lsp-rust-analyzer-display-parameter-hints t
          lsp-rust-analyzer-max-inlay-hint-length 20
          lsp-rust-analyzer-proc-macro-enable t
          lsp-rust-analyzer-diagnostics-enable-experimental t
          lsp-rust-analyzer-experimental-proc-attr-macros t
          lsp-rust-analyzer-import-granularity "crate"
          lsp-rust-analyzer-call-info-full t
          lsp-rust-analyzer-cargo-run-build-scripts t
          lsp-rust-analyzer-check-all-targets nil))


(after! lsp-javascript
  (defcustom-lsp lsp-typescript-jsx-completion-style "braces"
                 "JSX quoting style"
                 :group 'lsp-typescript
                 :lsp-path "typescript.preferences.jsxAttributeCompletionStyle")

  (setq! lsp-javascript-display-enum-member-value-hints t
          lsp-javascript-display-parameter-name-hints 'literals
          lsp-javascript-display-variable-type-hints t))

(add-hook! prog-mode
  (rainbow-delimiters-mode))

(add-hook! tsx-ts-mode
  (rainbow-delimiters-mode -1))

(after! engrave-faces
  (setq! engrave-faces-attributes-of-interest
          '(:foreground :slant :weight :height :strike-through)))

(setq! frame-title-format (list "%b - " (user-login-name) "@" (system-name)))

(after! haskell-mode
  (setq! haskell-auto-insert-module-format-string "module %s\n    (\n     ) where"))

(after! evil
  (setq! ;; evil-normal-state-cursor '(box "light blue")
   ;; evil-insert-state-cursor '(bar "medium sea green")
   ;; evil-visual-state-cursor '(hollow "orange")
   evil-want-fine-undo t
   evil-kill-on-visual-paste nil)
  (setq! evil-vsplit-window-right t
          evil-split-window-below t)
  ;; stops the evil selection being added to the kill-ring
  (fset 'evil-visual-update-x-selection 'ignore)
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (consult-buffer))
  (advice-add 'evil-ex-search-next :after
              (lambda (&rest _x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-ex-search-previous :after
              (lambda (&rest _x) (evil-scroll-line-to-center (line-number-at-pos))))
  (evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
    (interactive "<R><y>")
    (let ((text (replace-regexp-in-string "\n" "" (filter-buffer-substring beg end))))
      (if (string-match-p "^\\s-*$" text)
          (evil-delete beg end type ?_)
        (evil-delete beg end type reg yank-handler)))))

(after! consult
  (consult-customize
   +vertico/project-search consult-ripgrep consult-git-grep consult-grep
   +vertico-file-search
   +default/search-project
   :preview-key (list :debounce 0.5 'any))
  (setq! consult-async-refresh-delay 0.1
          consult-async-input-throttle 0.1
          consult-async-input-debounce 0.05))

(setq x-stretch-cursor t
      uniquify-buffer-name-style 'forward)

(setq! projectile-require-project-root t)

(setq! posframe-mouse-banish nil)

(setq! display-line-numbers-type nil)

(tooltip-mode t)
(global-subword-mode 1)

(setq! blink-matching-paren t
        blink-matching-paren-highlight-offscreen t)


(set-popup-rule! "^\\*Man"
  :side 'right
  :size 0.35)

(set-popup-rule! "^\\*Compile-Log"
  :ignore t)

(set-popup-rule! "^\\*odict"
  :side 'bottom
  :size 0.4
  :ttl 60)

;; (add-to-list 'auto-mode-alist '("\\.eex$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.j2$" . web-mode))

(after! web-mode
  (setq! web-mode-enable-inlays t
          web-mode-enable-current-element-highlight t
          web-mode-enable-html-entities-fontification t
          web-mode-enable-auto-closing t
          web-mode-enable-auto-opening t
          web-mode-enable-auto-pairing t
          web-mode-auto-quote-style 3
          web-mode-enable-auto-quoting t))

(setq! +treemacs-git-mode 'deferred)

(after! transient
  (setq transient-default-level 7))

(after! treemacs
  (treemacs-follow-mode +1)
  (set-popup-rule! "^ \\*Treemacs-Scoped-Buffer-[^*]*\\*" :ignore t)
  (setq! treemacs-silent-refresh t
          treemacs-read-string-input 'from-minibuffer))
(after! forge
  ;; (advice-remove 'forge-get-repository '+magit--forge-get-repository-lazily-a)
  ;; (advice-remove 'forge-dispatch '+magit--forge-build-binary-lazily-a)
  ;; (if (atom forge-topic-list-limit)
  ;;     (setq! forge-topic-list-limit (cons forge-topic-list-limit -5))
  ;;   (setcdr forge-topic-list-limit -5))

  (add-to-list 'transient-levels '(forge-dispatch (t . 7)))

  (transient-append-suffix 'magit-branch '(3 2 0)
    '("i" "new for issue" forge-create-branch-for-issue))

  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues :append t))
;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-notifications :append t))
;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues :append t)
;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-authored-pullreqs :append t)
;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-requested-reviews :append t))


;; (defun cc-bytecomp-is-compiling (&rest _))

(after! smartparens
  (sp-local-pair 'python-mode "f\"" "\"" :trigger "f\"" :post-handlers '(:add sp-python-fix-tripple-quotes))
  (sp-local-pair 'python-mode "f'" "'"))


(require 'zone)
(require 'zone-matrix-wake-up)

(defgroup zone-rise-and-shine nil
  "Rise and shine, Mr. Freeman."
  :group 'zone
  :prefix "zone-rise-and-shine-")

(defface zone-rise-and-shine-face
  '((t (:bold t :foreground "#c5c5c5")))
  "bleh."
  :group 'zone-rise-and-shine)

(defun zone-pgm-rise-and-shine ()
  "Zone out mr freeman."
  (delete-other-windows)
  (let ((msgs `((("Rise and shine, " 500) ("Mr. Freeman. " 500) ("Rise and shine..." 2000))
                (("Not that I wish to imply you have been sleeping on the job." 500)
                 ("\nNo one is more deserving of a rest. " 500)
                 ("\nAnd all the effort in the world would have gone to waste until..." 500)
                 ("\nwell, let's just say your hour has come again." 3000))
                (("The right man in the wrong place can make all the difference in the world." 2000))
                (("So, " 300) ("wake up, " 300) ("Mr. Freeman. " 500)
                 ("Wake up and smell the ashes." 5000)))))

    (sit-for 2)
    (cl-loop for msg in msgs do
             (erase-buffer)
             (cl-loop for (chunk end-delay) in msg do
                      (cl-loop for char across chunk do
                               (let ((s (string char)))
                                 (put-text-property 0 1 'face 'zone-rise-and-shine-face s)
                                 (insert s))
                               (sit-for (/ 100 1000.0)))
                      (sit-for (/ end-delay 1000.0)))))
  (sit-for 60))

(setq! zone-programs [zone-pgm-drip zone-pgm-rise-and-shine zone-pgm-matrix-wake-up zone-pgm-putz-with-case zone-pgm-five-oclock-swan-dive])

(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive
   (list
    (completing-read
     "Program: "
     (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (vector (intern pgm))))
    (zone)))

(when IS-MAC
  (global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#"))))

(when (not IS-MAC)
  (zone-when-idle 560))

;; (use-package! fussy
;;   :config
;;   (defun nospace-fussy-all-completions (string table pred point)
;;     "Get flex-completions of STRING in TABLE, given PRED and POINT."
;;     (unless (string-search " " string)
;;       (fussy-all-completions string table pred point)))

;;   (setq! fussy-use-cache t
;;         fussy-score-fn 'fussy-hotfuzz-score
;;         fussy-score-ALL-fn 'fussy-score
;;         fussy-filter-fn 'fussy-filter-default)
;;   (add-to-list 'completion-styles-alist
;;                '(fussy-nospace fussy-try-completions nospace-fussy-all-completions
;;                        "Smart Fuzzy completion with scoring."))
;;   (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
;;   (add-hook 'corfu-mode-hook
;;             (lambda ()
;;               (setq!-local fussy-max-candidate-limit 5000
;;                           fussy-default-regex-fn 'fussy-pattern-default
;;                           fussy-prefer-prefix t))))

(use-package! hotfuzz
  :config
  (defun nospace-hotfuzz-all-completions (string table &optional pred point)
    "Get hotfuzz-completions of STRING in TABLE.
        See `completion-all-completions' for the semantics of PRED and POINT.
        This function prematurely sorts the completions; mutating the result
        before passing it to `display-sort-function' or `cycle-sort-function'
        will lead to inaccuracies."
    (unless (string-search " " string)
      (hotfuzz-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(hotfuzz-nospace completion-flex-try-completion nospace-hotfuzz-all-completions
                         "Fuzzy completion."))
  (put 'hotfuzz-nospace 'completion--adjust-metadata #'hotfuzz--adjust-metadata))

(use-package! nucleo)

(setq! completion-ignore-case t)

(after! vertico
  (cl-defun +vertico-file-search--sort-a (orig-fn &key query in all-files (recursive t) prompt args)
    (let ((fixed-args (append args '("--sort" "path" "--max-filesize" "1000000"))))
      (funcall orig-fn :query query :in in :all-files all-files :recursive recursive :prompt prompt :args fixed-args)))

  (advice-add '+vertico-file-search :around #'+vertico-file-search--sort-a))


(defun set-completion-desires ()
  (setq! completion-category-overrides '())
  (add-to-list 'completion-category-overrides
               '(file (styles nucleo orderless)))
  (add-to-list 'completion-category-overrides
               '(project-file (styles nucleo orderless)))
  (add-to-list 'completion-category-overrides
               '(buffer (styles nucleo orderless)))
  (add-to-list 'completion-category-overrides
               '(lsp-capf (styles nucleo orderless))))

(set-completion-desires)

(after! orderless
  (set-completion-desires))
(after! corfu
  (set-completion-desires))

(after! vertico-repeat
  ;; added as vertico/consult is adding the # from ripgrep back after resuming
  (defun vertico--trim-hash-prefix (session)
    (when (cadr session)
      (setf (cadr session) (string-remove-prefix "#" (cadr session))))
    session)

  (add-to-list 'vertico-repeat-transformers #'vertico--trim-hash-prefix))

(after! corfu
  (require 'cape)
  (setq! global-corfu-minibuffer nil))

(use-package! corfu-echo
  :after corfu
  :hook (corfu-mode . corfu-echo-mode))
;;
;; (use-package! orderless
;;   :after corfu
;;   :config
;;   (setq! completion-styles '( orderless basic)))

(after! marginalia)
;; (setq! marginalia-censor-variables nil)

;; (defadvice! +marginalia--anotate-local-file-colorful (cand)
;;   "Just a more colourful version of `marginalia--anotate-local-file'."
;;   :override #'marginalia--annotate-local-file
;;   (when-let (attrs (file-attributes (substitute-in-file-name
;;                                      (marginalia--full-candidate cand))
;;                                     'integer))
;;     (marginalia--fields
;;      ((marginalia--file-owner attrs)
;;       :width 12 :face 'marginalia-file-owner)
;;      ((marginalia--file-modes attrs))
;;      ((+marginalia-file-size-colorful (file-attribute-size attrs))
;;       :width 7)
;;      ((+marginalia--time-colorful (file-attribute-modification-time attrs))
;;       :width 12))))

;; (defun +marginalia--time-colorful (time)
;;   (let* ((seconds (float-time (time-subtract (current-time) time)))
;;          (color (doom-blend
;;                  (face-attribute 'marginalia-date :foreground nil t)
;;                  (face-attribute 'marginalia-documentation :foreground nil t)
;;                  (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
;;     ;; 1 - log(3 + 1/(days + 1)) % grey
;;     (propertize (marginalia--time time) 'face (list :foreground color))))

;; (defun +marginalia-file-size-colorful (size)
;;   (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
;;          (color (if (< size-index 10000000) ; 10m
;;                     (doom-blend 'orange 'green size-index)
;;                   (doom-blend 'red 'orange (- size-index 1)))))
;;     (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(use-package! indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq!
                                        ;indent-bars-prefer-character (eq (window-system) 'ns)
   indent-bars-pattern "."
   indent-bars-width-frac 0.20
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.5)
   indent-bars-display-on-blank-lines t))


(after! doom-modeline
  (setq! doom-modeline-height 23))

(defun auth-source-1password--1password-construct-query-path-escaped (_backend _type host user _port)
  "Construct the full entry-path for the 1password entry for HOST and USER.
   Usually starting with the `auth-source-1password-vault', followed
   by host and user."
  (mapconcat #'identity (list auth-source-1password-vault host (string-replace "^" "_" user)) "/"))

(use-package! auth-source-1password
  :custom
  (auth-source-1password-vault "Auto")
  (auth-source-1password-construct-secret-reference #'auth-source-1password--1password-construct-query-path-escaped)
  :config
  (auth-source-1password-enable))

(use-package! typst-ts-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.typ" . typst-ts-mode))
  :custom
  (typst-ts-mode-watch-options "--open"))
;;
(setq! mac-command-modifier 'meta
        mac-option-modifier nil)

(after! cape
  (setq! cape-dabbrev-check-other-buffers nil))

(setq! mac-mouse-wheel-smooth-scroll t)

(use-package! ultra-scroll
  :config (ultra-scroll-mode 1))

(when (fboundp 'pixel-scroll-precision-mode)

  (setq! pixel-scroll-precision-interpolate-page t))

(custom-set-faces!
  '(org-level-1 :inherit (fixed-pitch-serif outline-1))
  '(org-level-2 :inherit (fixed-pitch-serif outline-2))
  '(org-level-3 :inherit (fixed-pitch-serif outline-3))
  '(org-level-4 :inherit (fixed-pitch-serif outline-4))
  '(org-level-5 :inherit (fixed-pitch-serif outline-5))
  '(org-level-6 :inherit (fixed-pitch-serif outline-6))
  '(org-level-7 :inherit (fixed-pitch-serif outline-7))
  '(org-level-8 :inherit (fixed-pitch-serif outline-8))
  '(org-verse :inherit (fixed-pitch-serif))
  '(org-verbatim :inherit (fixed-pitch))
  '(org-quote :inherit (fixed-pitch-serif)))

(after! org-mode
  (add-hook! org-mode #'+word-wrap-mode)
  (defun org-do-latex-and-related (&rest _)))

(setq! org-modern-star nil
       org-modern-table nil
       org-modern-timestamp nil
       org-modern-tag nil
       org-modern-hide-stars nil)

(use-package! ox-typst
  :after org)

;; (use-package! org-flyimage
;;   :defer t
;;   :hook (org-mode . org-flyimage-mode))

;; (use-package! org-limit-image-size
;;   :defer t
;;   :hook (org-mode . org-limit-image-size-activate))

;; (use-package! org-tidy
;;   :defer t
;;   :hook (org-mode . org-tidy-mode))
;; (use-package! org-hide-tags
;;   :defer t
;;   :hook (org-mode . org-hide-tags-mode))

(setq! window-combination-resize t
        mouse-drag-and-drop-region-cross-program t
        scroll-margin 0)

(setq! parinfer-rust-disable-troublesome-modes t)

(setq! pdf-tools-installer-os "nixos")

(setq! doom-theme 'doom-lantern)
;; (setq! doom-theme 'doom-opera-light
;;        frame-background-mode 'light)
;; (setq! doom-theme 'doom-opera-light)

(use-package! auto-dark)

(after! doom-ui
  (setq! auto-dark-allow-osascript t
         auto-dark-themes `((,doom-theme) (doom-one-light)))
  (auto-dark-mode 1))

(after! markdown
  (setq! markdown-fontify-code-blocks-natively t))

(use-package! scad-mode
  :defer t
  :mode "\\.scad\\'")

(use-package! nushell-ts-mode
  :defer t
  :mode "\\.nu\\'")

(after! dtrt-indent
  (add-to-list 'dtrt-indent-hook-mapping-list '(scad-mode c/c++/java c-basic-offset)))

;; (dtrt-indent-global-mode +1)

(setq! flyspell-delay-use-timer t)
(setq! rust-ts-mode-fontify-number-suffix-as-type t)

;; (use-package! jujutsu)

;; (use-package! difftastic
;;   :defer t
;;   :config (difftastic-bindings-mode))

(after! treesit
  (setq! treesit-font-lock-level 4)
                                        ;indent-bars-treesit-support t)
  (add-hook! haskell-ts-mode
    (advice-add #'comment-forward :around #'+haskell-ts--inhibit-forward-comment)))
;; (setq-local +default-want-RET-continue-comments nil
;;             +evil-want-o/O-to-continue-comments nil)))

                                        ;(defun th/magit--with-difftastic (buffer command)
                                        ;  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
                                        ;  (let ((process-environment
                                        ;         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                                        ;                       (number-to-string (frame-width)))
                                        ;               process-environment)))
                                        ;    ;; Clear the result buffer (we might regenerate a diff, e.g., for
                                        ;    ;; the current changes in our working directory).
                                        ;    (with-current-buffer buffer
                                        ;      (setq! buffer-read-only nil)
                                        ;      (erase-buffer))
                                        ;    ;; Now spawn a process calling the git COMMAND.
                                        ;    (make-process
                                        ;     :name (buffer-name buffer)
                                        ;     :buffer buffer
                                        ;     :command command
                                        ;     ;; Don't query for running processes when emacs is quit.
                                        ;     :noquery t
                                        ;     ;; Show the result buffer once the process has finished.
                                        ;     :sentinel (lambda (proc event)
                                        ;                 (when (eq (process-status proc) 'exit)
                                        ;                   (with-current-buffer (process-buffer proc)
                                        ;                     (goto-char (point-min))
                                        ;                     (ansi-color-apply-on-region (point-min) (point-max))
                                        ;                     (setq! buffer-read-only t)
                                        ;                     (view-mode)
                                        ;                     (end-of-line)
                                        ;                     ;; difftastic diffs are usually 2-column side-by-side,
                                        ;                     ;; so ensure our window is wide enough.
                                        ;                     (let ((width (current-column)))
                                        ;                       (while (zerop (forward-line 1))
                                        ;                         (end-of-line)
                                        ;                         (setq! width (max (current-column) width)))
                                        ;                       ;; Add column size of fringes
                                        ;                       (setq! width (+ width
                                        ;                                      (fringe-columns 'left)
                                        ;                                      (fringe-columns 'right)))
                                        ;                       (goto-char (point-min))
                                        ;                       (pop-to-buffer
                                        ;                        (current-buffer)
                                        ;                        `(;; If the buffer is that wide that splitting the frame in
                                        ;                          ;; two side-by-side windows would result in less than
                                        ;                          ;; 80 columns left, ensure it's shown at the bottom.
                                        ;                          ,(when (> 80 (- (frame-width) width))
                                        ;                             #'display-buffer-at-bottom)
                                        ;                          (window-width
                                        ;                           . ,(min width (frame-width))))))))))))
                                        ;
                                        ;(defun th/magit-show-with-difftastic (rev)
                                        ;  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
                                        ;  (interactive
                                        ;   (list (or
                                        ;          ;; If REV is given, just use it.
                                        ;          (when (boundp 'rev) rev)
                                        ;          ;; If not invoked with prefix arg, try to guess the REV from
                                        ;          ;; point's position.
                                        ;          (and (not current-prefix-arg)
                                        ;               (or (magit-thing-at-point 'git-revision t)
                                        ;                   (magit-branch-or-commit-at-point)))
                                        ;          ;; Otherwise, query the user.
                                        ;          (magit-read-branch-or-commit "Revision"))))
                                        ;  (if (not rev)
                                        ;      (error "No revision specified")
                                        ;    (th/magit--with-difftastic
                                        ;     (get-buffer-create (concat "*git show difftastic " rev "*"))
                                        ;     (list "git" "--no-pager" "show" "--ext-diff" rev))))
                                        ;
                                        ;(defun th/magit-diff-with-difftastic (arg)
                                        ;  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
                                        ;  (interactive
                                        ;   (list (or
                                        ;          ;; If RANGE is given, just use it.
                                        ;          (when (boundp 'range) range)
                                        ;          ;; If prefix arg is given, query the user.
                                        ;          (and current-prefix-arg
                                        ;               (magit-diff-read-range-or-commit "Range"))
                                        ;          ;; Otherwise, auto-guess based on position of point, e.g., based on
                                        ;          ;; if we are in the Staged or Unstaged section.
                                        ;          (pcase (magit-diff--dwim)
                                        ;            ('unmerged (error "unmerged is not yet implemented"))
                                        ;            ('unstaged nil)
                                        ;            ('staged "--cached")
                                        ;            (`(stash . ,value) (error "stash is not yet implemented"))
                                        ;            (`(commit . ,value) (format "%s^..%s" value value))
                                        ;            ((and range (pred stringp)) range)
                                        ;            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
                                        ;  (let ((name (concat "*git diff difftastic"
                                        ;                      (if arg (concat " " arg) "")
                                        ;                      "*")))
                                        ;    (th/magit--with-difftastic
                                        ;     (get-buffer-create name)
                                        ;     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(defun treesit-language-at (position)
  "Return the language at POSITION.

When there are multiple parsers that covers POSITION, determine
the most relevant parser (hence language) by their embed level.
If `treesit-language-at-point-function' is non-nil, return
the return value of that function instead."
  (if treesit-language-at-point-function
      (funcall treesit-language-at-point-function position)
    (let ((parser (car (treesit-parsers-at position))))
      (if parser
          (treesit-parser-language parser)))))


;; (defun +lookup/dictionary-definition (identifier &optional arg)
;;   "Look up the definition of the word at point (or selection)."
;;   (interactive
;;    (list (or (if (equal major-mode 'pdf-view-mode)
;;                  (car (pdf-view-active-region-text)))
;;              (doom-thing-at-point-or-region 'word)
;;              (read-string "Look up in dictionary: "))
;;          current-prefix-arg))
;;   (message "Looking up dictionary definition for %S" identifier)
;;   (cond ((and (featurep :system 'macos) (require 'osx-dictionary nil t))
;;          (osx-dictionary--view-result identifier))
;;         ((and +lookup-dictionary-prefer-offline
;;               (require 'wordnut nil t))
;;          (unless (executable-find wordnut-cmd)
;;            (user-error "Couldn't find %S installed on your system"
;;                        wordnut-cmd))
;;          (wordnut-search identifier))
;;         ((require 'define-word nil t)
;;          (define-word identifier nil arg))
;;         ((user-error "No dictionary backend is available"))))

(setq! pdf-view-selection-style 'glyph)

(setq! odict-dictionaries (list
                           (f-canonical (f-join "~" "Dropbox" "dictionaries" "deutsch.odict")))
        odict-default-dictionary "deutsch")

(add-hook! org-mode-hook
  (defun org-typst-node-property (node-property _contents _info)
    (format "%s:%s" (org-element-property :key node-property)
            (let ((value (org-element-property :value node-property)))
              (if value (concat " " value) ""))))

  (defun org-typst-verse-block (verse-block contents info)
    contents))
;; (org-typst--raw contents verse-block info nil t))

(setq! jinx-languages "en_GB")

(add-hook! yaml-mode (lsp!))

(use-package! typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!))

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(use-package! ast-grep)

(after! (nerd-icons haskell-ts-mode)
  (add-to-list 'nerd-icons-mode-icon-alist '(haskell-ts-mode nerd-icons-devicon "nf-dev-haskell" :face nerd-icons-red)))
