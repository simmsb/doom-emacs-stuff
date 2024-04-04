;;; private/ben/config.el -*- lexical-binding: t; -*-

(setq ON-DESKTOP (string= (system-name) "home"))
(setq ON-LAPTOP (string= (system-name) "laptop"))

(require 'cl-lib)

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
         doom-variable-pitch-font (font-spec :family "Fira Sans")
         doom-serif-font (font-spec :family "Fira Code" :size 14)))
  ("worklaptop"
   (setq doom-font (first-font
                    (font-spec :family "MonoLisa" :size 14)
                    (font-spec :family "Fira Code" :size 14))
         doom-big-font (first-font
                        (font-spec :family "Fira Code" :size 28))
         doom-variable-pitch-font (font-spec :family "Fira Sans")
         doom-serif-font (font-spec :family "Fira Code" :size 14))))
  

(print doom-font)

;; bindings
(map!
 (:leader
  (:prefix "f"
   :desc "Toggle Treemacs" "t" #'+treemacs/toggle-project)
  (:prefix "o"
   :desc "Open Shopping" "s" #'org-shopping-open
   :desc "Open kill ring" "k" #'+default/yank-pop
   :desc "Open notes.org" "n" #'org-notes-open))

 (:map evilem-map
  :after evil-easymotion
  "<down>" #'evilem-motion-next-line
  "<up>" #'evilem-motion-previous-line)

 (:map evil-window-map
       "<left>"     #'evil-window-left
       "<right>"    #'evil-window-right
       "<up>"       #'evil-window-up
       "<down>"     #'evil-window-down)

 ;; in lisp modes use default evil-delete for parinfer magic
 ;; (:mode (emacs-lisp-mode clojure-mode scheme-mode lisp-mode)
 ;;  :i "<backspace>" #'parinfer-backward-delete-char
 ;;  :i "C-d" #'delete-char)

 "<home>" #'back-to-indentation-or-beginning
 "<end>" #'end-of-line
 "s-q" #'prog-fill-reindent-defun
 "<f6>" #'evil-switch-to-windows-last-buffer)

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
  (setq geros-eval-result-duration nil)
  :hook
  (geiser-mode . geros-mode))

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

(after! lsp-haskell

  (defcustom-lsp lsp-haskell-plugin-rename-on
    t
    "Enables rename plugin"
    :group 'lsp-haskell-plugins
    :type 'boolean
    :lsp-path "haskell.plugin.rename.globalOn")

  (setq lsp-haskell-formatting-provider "ormolu"
        lsp-haskell-server-args '("+RTS" "-N8" "-RTS")
        lsp-haskell-plugin-ghcide-type-lenses-config-mode "exported"
        lsp-haskell-tactics-on nil
        lsp-haskell-max-completions 20)
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))

;; no idea mate
(after! browse-url
  (defun browse-url (url)
    (browse-url-firefox url)))

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
  (lsp-ui-mode +1)
  (setq lsp-lens-enable nil
        lsp-inlay-hint-enable t
        lsp-modeline-diagnostics-scope :project
        lsp-restart 'auto-restart
        lsp-enable-indentation t
        lsp-enable-file-watchers t
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-diagnostic-max-lines 10)
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
  (remove-hook 'magit-status-sections-hook 'forge-insert-issues)
  (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))



(after! flycheck
  (add-hook! haskell-mode
    (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-hlint)))

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
              (lambda (&rest _x) (evil-scroll-line-to-center (line-number-at-pos)))))

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
  (advice-remove 'forge-get-repository '+magit--forge-get-repository-lazily-a)
  (advice-remove 'forge-dispatch '+magit--forge-build-binary-lazily-a)
  (if (atom forge-topic-list-limit)
      (setq forge-topic-list-limit (cons forge-topic-list-limit -5))
    (setcdr forge-topic-list-limit -5))

  (add-to-list 'transient-levels '(forge-dispatch (t . 7)))

  (transient-append-suffix 'magit-branch '(3 2 0)
    '("i" "new for issue" forge-create-branch-for-issue)))


;; (defun cc-bytecomp-is-compiling (&rest _))

(after! smartparens
  (sp-local-pair 'python-mode "f\"" "\"" :trigger "f\"" :post-handlers '(:add sp-python-fix-tripple-quotes))
  (sp-local-pair 'python-mode "f'" "'"))


(if (string-equal (system-name) "worklaptop")
    (setq doom-theme 'doom-oceanic-next)
  (setq doom-theme 'doom-lantern))

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
  (require 'cape))

(setq +corfu-want-minibuffer-completion 'agressive)

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

;; (use-package! typst-ts-mode
;;   :defer t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.typ" . typst-ts-mode))
;;   :custom
;;   (typst-ts-mode-watch-options "--open"))
;;
(setq mac-command-modifier 'meta)

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
