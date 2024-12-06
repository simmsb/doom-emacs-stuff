;;; private/ben/config.el -*- lexical-binding: t; -*-

(setq ON-DESKTOP (string= (system-name) "home"))
(setq ON-LAPTOP (string= (system-name) "laptop"))

(require 'cl-lib)
(require 'f)

(defun first-font (&rest fonts)
  (cl-find-if #'find-font fonts))

(pcase (system-name)
  ("home"
   (setq doom-font (first-font
                    (font-spec :family "MonoLisa" :size 15)
                    (font-spec :family "Fira Code" :size 15))
         doom-big-font (first-font
                        (font-spec :family "MonoLisa" :size 22)
                        (font-spec :family "Fira Code" :size 22))
         doom-variable-pitch-font (font-spec :family "Fira Sans")
         doom-serif-font (font-spec :family "Fira Code" :size 16)))
  ("laptop2"
   (setq doom-font (first-font
                    (font-spec :family "MonoLisa" :size 14)
                    (font-spec :family "Fira Code" :size 14))
         doom-big-font (first-font
                        (font-spec :family "Fira Code" :size 28))
         doom-serif-font (font-spec :family "Latin Modern Mono" :size 18)))
  ("worklaptop"
   (setq doom-font (first-font
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
   :desc "Search project (affe)" "P" #'affe-grep))

 (:map evilem-map
  :after evil-easymotion
  "<down>" #'evilem-motion-next-line
  "<up>" #'evilem-motion-previous-line)

 (:map evil-window-map
       "<left>"     #'evil-window-left
       "<right>"    #'evil-window-right
       "<up>"       #'evil-window-up
       "<down>"     #'evil-window-down)

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
  (setq input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))

(use-package! affe
  :after '(orderless)
  :custom
  ((affe-regexp-compiler . #'affe-orderless-regexp-compiler)))

(defun do-nothing (&rest _)
  (interactive)
  "Does nothing.")

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
  (setq geros-eval-result-duration nil))

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
;;   (setq lsp-copilot-user-languages-config (f-join doom-user-dir "languages.toml"))
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

  (defcustom-lsp lsp-haskell-plugin-rename-on
    t
    "Enables rename plugin"
    :group 'lsp-haskell-plugins
    :type 'boolean
    :lsp-path "haskell.plugin.rename.globalOn")

  (setq lsp-haskell-formatting-provider "ormolu"
        lsp-haskell-server-args '("+RTS" "-N8" "-RTS")
        lsp-haskell-plugin-ghcide-type-lenses-config-mode "always"
        lsp-haskell-tactics-on nil
        lsp-haskell-max-completions 40
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
  (setq nix-nixfmt-bin "nixpkgs-fmt"))
;; (set-formatter! 'nixpkgs-fmt "nixpkgs-fmt" :modes 'nix-mode))

(when ON-DESKTOP
  (use-package! discord-emacs)
  (run-at-time "1 min" nil #'discord-emacs-run "384815451978334208"))


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
  (setq lsp-lens-enable nil
        +lsp-defer-shutdown nil
        lsp-inlay-hint-enable t
        lsp-modeline-diagnostics-scope :project
        lsp-restart 'auto-restart
        lsp-enable-indentation t
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

(after! magit
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

(after! flycheck
  (add-hook! haskell-mode
    (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))
  (setq flycheck-popup-tip-error-prefix "❌ "))

(after! flycheck-posframe
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-error-prefix "❌ "
        flycheck-posframe-info-prefix "ⓘ "))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        rustic-treesitter-derive t))

(after! lsp-rust
  (setq lsp-rust-analyzer-display-chaining-hints nil
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

  (setq lsp-javascript-display-enum-member-value-hints t
        lsp-javascript-display-parameter-name-hints 'literals
        lsp-javascript-display-variable-type-hints t))

(add-hook! prog-mode #'rainbow-delimiters-mode)

(after! engrave-faces
  (setq engrave-faces-attributes-of-interest
        '(:foreground :slant :weight :height :strike-through)))

(setq frame-title-format (list "%b - " (user-login-name) "@" (system-name)))

(when ON-LAPTOP
  (after! disable-mouse
    (global-disable-mouse-mode)))

(after! haskell-mode
  (setq haskell-auto-insert-module-format-string "module %s\n    (\n     ) where"))


(after! evil
  (setq ;; evil-normal-state-cursor '(box "light blue")
   ;; evil-insert-state-cursor '(bar "medium sea green")
   ;; evil-visual-state-cursor '(hollow "orange")
   evil-want-fine-undo t
   evil-kill-on-visual-paste nil)
  (setq evil-vsplit-window-right t
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
   :preview-key '(:debounce 0.5 any)))

(setq-default x-stretch-cursor t
              uniquify-buffer-name-style 'forward)

(setq projectile-require-project-root t)

(setq posframe-mouse-banish nil)

(setq display-line-numbers-type nil)

(global-subword-mode 1)

(set-popup-rule! "^\\*Man"
  :side 'right
  :size 0.35)

;; (add-to-list 'auto-mode-alist '("\\.eex$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.j2$" . web-mode))

(after! web-mode
  (setq web-mode-enable-inlays t
        web-mode-enable-current-element-highlight t
        web-mode-enable-html-entities-fontification t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t
        web-mode-auto-quote-style 3
        web-mode-enable-auto-quoting t))

(setq +treemacs-git-mode 'deferred)

(after! treemacs
  (treemacs-follow-mode +1)
  (set-popup-rule! "^ \\*Treemacs-Scoped-Buffer-[^*]*\\*" :ignore t)
  (setq treemacs-silent-refresh t
        treemacs-read-string-input 'from-minibuffer))
(after! forge
  ;; (advice-remove 'forge-get-repository '+magit--forge-get-repository-lazily-a)
  ;; (advice-remove 'forge-dispatch '+magit--forge-build-binary-lazily-a)
  ;; (if (atom forge-topic-list-limit)
  ;;     (setq forge-topic-list-limit (cons forge-topic-list-limit -5))
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


(setq doom-theme 'doom-oceanic-next)

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

(setq zone-programs [zone-pgm-drip zone-pgm-rise-and-shine zone-pgm-matrix-wake-up zone-pgm-putz-with-case zone-pgm-five-oclock-swan-dive])

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

(use-package  flx-rs
  :config
  (setq fussy-score-fn 'fussy-flx-rs-score)
  (flx-rs-load-dyn))

(use-package! fussy
  :custom
  (fussy-use-cache t)
  (setq fussy-score-fn 'fussy-flx-rs-score)
  (fussy-filter-fn 'fussy-filter-default)
  :after '(corfu orderless)
  :config
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-default
                          fussy-prefer-prefix t))))

(after! vertico
  (setq completion-category-defaults '())
  (add-to-list 'completion-category-overrides
               '(file (styles +vertico-basic-remote fussy orderless)))
  (add-to-list 'completion-category-overrides
               '(project-file (styles orderless)))
  (add-to-list 'completion-category-overrides
               '(buffer (styles fussy orderless)))
  (add-to-list 'completion-category-overrides
               '(lsp-capf (styles basic partial-completion orderless))))

(after! corfu
  (require 'cape)
  (setq global-corfu-minibuffer nil))

(use-package! corfu-echo
  :after corfu
  :hook (corfu-mode . corfu-echo-mode))
;;
;; (use-package! orderless
;;   :after corfu
;;   :config
;;   (setq completion-styles '( orderless basic)))

(after! marginalia
  ;; (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(use-package! indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq
                                        ;indent-bars-prefer-character (eq (window-system) 'ns)
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   indent-bars-pattern "."
   indent-bars-width-frac 0.2
   indent-bars-pad-frac 0.2
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil))


(after! doom-modeline
  (setq doom-modeline-height 30))

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
(setq mac-command-modifier 'meta
      mac-option-modifier nil)

(after! cape
  (setq cape-dabbrev-check-other-buffers nil))

(use-package! ultra-scroll-mac
  :if (eq window-system 'mac)
  :init (setq scroll-conservatively 101)
  :config (ultra-scroll-mac-mode 1))

(when (and (not (eq window-system 'mac)) (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-interpolate-page t
        pixel-scroll-precision-use-momentum t))

(custom-set-faces!
  '(org-level-1 :inherit (fixed-pitch-serif outline-1))
  '(org-level-2 :inherit (fixed-pitch-serif outline-2))
  '(org-level-3 :inherit (fixed-pitch-serif outline-3))
  '(org-level-4 :inherit (fixed-pitch-serif outline-4))
  '(org-level-5 :inherit (fixed-pitch-serif outline-5))
  '(org-level-6 :inherit (fixed-pitch-serif outline-6))
  '(org-level-7 :inherit (fixed-pitch-serif outline-7))
  '(org-level-8 :inherit (fixed-pitch-serif outline-8))
  '(org-verse :inherit (shadow fixed-pitch-serif))
  '(org-verbatim :inherit (shadow fixed-pitch))
  '(org-quote :inherit (fixed-pitch-serif)))

(after! org-mode
  (add-hook! org-mode #'+word-wrap-mode)
  (defun org-do-latex-and-related (&rest _)))

(setq window-combination-resize t
      mouse-drag-and-drop-region-cross-program t
      scroll-margin 0)

(setq parinfer-rust-disable-troublesome-modes t)

(setq pdf-tools-installer-os "nixos")

(use-package! auto-dark)

(after! doom-ui
  (setq! auto-dark-allow-osascript t
         auto-dark-dark-theme 'doom-oceanic-next
         auto-dark-light-theme 'doom-tomorrow-day)
  (auto-dark-mode 1))

(after! markdown
  (setq markdown-fontify-code-blocks-natively t))

(use-package! scad-mode
  :defer t
  :mode "\\.scad\\'")

(after! dtrt-indent
  (add-to-list 'dtrt-indent-hook-mapping-list '(scad-mode c/c++/java c-basic-offset)))

(dtrt-indent-global-mode +1)

(setq flyspell-delay-use-timer t)
(setq rust-ts-mode-fontify-number-suffix-as-type t)
