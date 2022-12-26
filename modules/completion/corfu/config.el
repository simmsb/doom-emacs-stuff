;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; (defun company-mode ()
;;   (error "what"))

(use-package! corfu
  :custom
  (corfu-separator ?\s)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay 0.0)
  (completion-cycle-threshold 3)
  (corfu-min-width 80)
  (tab-always-indent 'complete)
  :hook (doom-first-input . global-corfu-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'+corfu--enable-in-minibuffer)
  (corfu-popupinfo-mode t)
  (add-hook 'doom-init-modules-hook
            (lambda ()
              (after! lsp-mode
                (setq lsp-completion-provider :none))))
  (add-hook 'lsp-mode-hook (defun doom--add-lsp-capf ()
                             (add-to-list 'completion-at-point-functions (cape-capf-buster #'lsp-completion-at-point)))
            ;; Below is so that context specific completions in cape come first.
            :depth 1)
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(orderless)))))
  ;; (add-to-list 'completion-styles 'partial-completion t)
  ;; (add-to-list 'completion-styles 'initials t))

(use-package! fzf-native
  :custom
  (fussy-score-fn 'fussy-fzf-native-score)
  :config
  (fzf-native-load-dyn))

(use-package! fussy
  :custom
  (fussy-use-cache t)
  (fussy-score-fn 'fussy-fzf-native-score)
  (fussy-filter-fn 'fussy-filter-default)
  :after corfu
  :config
  (setq completion-category-defaults '())
  (add-to-list 'completion-category-overrides
               '(file (styles +vertico-basic-remote fussy orderless)))
  (add-to-list 'completion-category-overrides
               '(project-file (styles fussy orderless)))
  (add-to-list 'completion-category-overrides
               '(buffer (styles fussy orderless)))
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 200
                          fussy-default-regex-fn 'fussy-pattern-default
                          fussy-prefer-prefix t))))

(use-package! corfu-echo
  :after corfu
  :hook (corfu-mode . corfu-echo-mode))


(use-package! corfu-info
  :after corfu)


(use-package! corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package! orderless
  :after corfu
  :config
  (setq completion-styles '(substring orderless basic)))

(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background t)
  (kind-icon-blend-frac 0.2)
  :config
  (setq kind-icon-use-icons t
        svg-lib-icons-dir (expand-file-name "svg-lib" doom-cache-dir)
        kind-icon-mapping
        `((array "a" :icon "code-brackets" :face font-lock-variable-name-face)
          (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
          (class "c" :icon "view-grid-plus-outline" :face font-lock-type-face)
          (color "#" :icon "palette" :face success)
          (constant "co" :icon "pause-circle" :face font-lock-constant-face)
          (constructor "cn" :icon "table-column-plus-after" :face font-lock-function-name-face)
          (enum "e" :icon "format-list-bulleted-square" :face font-lock-builtin-face)
          (enum-member "em" :icon "format-list-checks" :face font-lock-builtin-face)
          (event "ev" :icon "lightning-bolt-outline" :face font-lock-warning-face)
          (field "fd" :icon "application-braces-outline" :face font-lock-variable-name-face)
          (file "f" :icon "file" :face font-lock-string-face)
          (folder "d" :icon "folder" :face font-lock-doc-face)
          (function "f" :icon "sigma" :face font-lock-function-name-face)
          (interface "if" :icon "video-input-component" :face font-lock-type-face)
          (keyword "kw" :icon "image-filter-center-focus" :face font-lock-keyword-face)
          (macro "mc" :icon "lambda" :face font-lock-keyword-face)
          (method "m" :icon "sigma" :face font-lock-function-name-face)
          (module ,(string ?\{) :icon "view-module" :face font-lock-preprocessor-face)
          (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
          (operator "op" :icon "plus-circle-outline" :face font-lock-comment-delimiter-face)
          (param "pa" :icon "cog" :face default)
          (property "pr" :icon "tune-vertical" :face font-lock-variable-name-face)
          (reference "rf" :icon "bookmark-box-multiple" :face font-lock-variable-name-face)
          (snippet "S" :icon "text-short" :face font-lock-string-face)
          (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
          (struct "%" :icon "code-braces" :face font-lock-variable-name-face)
          (t "." :icon "crosshairs-question" :face shadow)
          (text "tx" :icon "script-text-outline" :face shadow)
          (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
          (unit "u" :icon "ruler-square" :face shadow)
          (value "v" :icon "numeric-1-box-multiple-outline" :face font-lock-builtin-face)
          (variable "va" :icon "adjust" :face font-lock-variable-name-face)))
  (add-hook 'doom-load-theme-hook #'kind-icon-reset-cache)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package! cape
  :defer t
  :init
  (map!
   [remap dabbrev-expand] 'cape-dabbrev)
  (add-hook! 'latex-mode-hook (defun +corfu--latex-set-capfs ()
                                (add-to-list 'completion-at-point-functions #'cape-tex)))
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (when (modulep! :checkers spell)
    ;; (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-ispell))
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))


(use-package! corfu-history
  :after corfu
  :hook (corfu-mode . (lambda ()
                        (corfu-history-mode 1)
                        (savehist-mode 1)
                        (add-to-list 'savehist-additional-variables 'corfu-history))))

(use-package! evil-collection-corfu
  :when (modulep! :editor evil +everywhere)
  :defer t
  :init (setq evil-collection-corfu-key-themes '(default magic-return))
  :config
  (evil-collection-corfu-setup))

(use-package! company-yasnippet
  :when (modulep! :editor snippets)
  :commands company-yasnippet
  :after cape
  :init
  (require 'company-yasnippet)
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (defalias 'yasnippet-capf
                (cape-company-to-capf #'company-yasnippet))
              (add-to-list 'completion-at-point-functions #'yasnippet-capf))))
