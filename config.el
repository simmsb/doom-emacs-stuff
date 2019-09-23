;;; private/ben/config.el -*- lexical-binding: t; -*-

;; bindings

;; why?
(package-initialize)

(map!
 (:leader
   (:prefix "f"
     :desc "Toggle Treemacs" "t" #'+treemacs/toggle)
   (:prefix "o"
     :desc "Open Shopping" "s" #'org-shopping-open
     :desc "Open kill ring" "k" #'helm-show-kill-ring)
   (:prefix "i"
     :desc "Insert UCS schar" "c" #'helm-ucs))

 (:after evil-multiedit
   (:prefix "gz"
     :nv "z" #'evil-multiedit-toggle-marker-here))

 (:map evilem-map
   :after evil-easymotion
   "<down>" #'evilem-motion-next-line
   "<up>" #'evilem-motion-previous-line)

 (:map evil-window-map
   "<left>"     #'evil-window-left
   "<right>"    #'evil-window-right
   "<up>"       #'evil-window-up
   "<down>"     #'evil-window-down)

 (:map evil-motion-state-map
   "?" #'counsel-grep-or-swiper)

 ;; in lisp modes use default evil-delete for parinfer magic
 (:mode (emacs-lisp-mode clojure-mode scheme-mode lisp-mode)
   :i "<backspace>" #'parinfer-backward-delete-char
   :i "C-d" #'delete-char)

 :i "<backspace>" #'smart-hungry-delete-backward-char
 :i "C-d" #'smart-hungry-delete-forwards-char

 "<home>" #'back-to-indentation-or-beginning
 "<end>" #'end-of-line)

;; (use-package! elpy)
;; (use-package! rainbow-identifiers)
(use-package! disable-mouse)
(use-package! clang-format)
(use-package! popup-kill-ring)
(use-package! transpose-frame)
(use-package! evil-anzu)
(use-package! github-review)
(use-package! github-browse-file)
(use-package! emojify)
(use-package! futhark-mode)

(use-package! geros
  :config
  (setq geros-eval-result-duration nil)
  :hook
  (geiser-mode . geros-mode))

(use-package! ox-hugo
  :after ox)


(use-package! evil-lion
  :config
  (evil-lion-mode))

(use-package! drag-stuff
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package! sqlup-mode
  :commands (sqlup-mode)
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(use-package! anzu
  :config
  (global-anzu-mode +1))

(use-package! smart-hungry-delete
  ;; :ensure t
  :defer nil
  :config (smart-hungry-delete-add-default-hooks))

(use-package! backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package! esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package! lsp-haskell
  :after haskell-mode
  :hook (haskell-mode . lsp-deferred)
  :config
  (lsp-haskell-set-completion-snippets-off)
  (lsp-haskell-set-config "formattingProvider" "floskell")
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))

(setq ON-DESKTOP (string= (system-name) "desktop"))
(setq ON-LAPTOP (string= (system-name) "laptop"))

(when ON-DESKTOP
  (use-package! discord-emacs)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  (run-at-time "1 min" nil #'discord-emacs-run "384815451978334208"))

(after! lsp
  (require 'yasnippet)
  (setq lsp-enable-xref t
        lsp-enable-completion-at-point nil
        lsp-enable-snippet t
        lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))

  (set-formatter! 'lsp-formatter #'lsp-format-buffer
    :modes '(lsp-mode)))

(after! company-lsp
  (setq company-lsp-enable-snippet t)

  (add-hook! (python-mode haskell-mode) (setq company-lsp-enable-snippet t)))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-header t
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-border (doom-color 'fg)))

(after! magit
  (magit-wip-mode 1))

(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc))

(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-quickhelp-delay 0.4
        company-backends '((company-yasnippet
                            company-keywords
                            company-capf)))

  (set-company-backend! 'org-mode
    '(company-math-symbols-latex
      company-latex-commands)
    '(company-files
      company-yasnippet
      company-keywords
      company-capf)
    '(company-abbrev
      company-dabbrev))

  (company-quickhelp-mode)
  (global-company-mode))

;; (after! rainbow-identifiers
;;   (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(setq display-line-numbers nil)
(setq doom-line-numbers-style nil)
(global-display-line-numbers-mode -1)

(add-hook! display-line-numbers-mode (global-display-line-numbers-mode -1))

(add-hook! prog-mode #'rainbow-delimiters-mode)

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ditaa . t)
     (sql . t)))
  (setq org-tags-column 100)
  (setq org-latex-packages-alist
        '(("" "physics" t)))
  (setq org-sticky-header-full-path 'full)

  (add-hook! org-mode
    (org-sticky-header-mode 1))

  (add-to-list 'org-src-lang-modes '("rust" . rustic))

  (setq org-attach-screenshot-command-line "escrotum -s %f")
  (setq org-reveal-root "~/dev/reveal.js")
  (setq org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0))

(setq org-log-done 'time
      +org-default-notes-file (f-join org-directory "notes.org")
      +org-default-todo-file (f-join org-directory "todo.org")
      +org-default-calendar-file (f-join org-directory "calendar.org")
      +org-shopping-file (f-join org-directory "shopping_list.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file +org-default-todo-file)
         "* [ ] %?\n%i" :prepend t :kill-buffer t)
        ("n" "Notes" entry (file +org-default-notes-file)
         "* %u %?\n%i" :prepend t :kill-buffer t)
        ("c" "Calendar" entry (file +org-default-calendar-file)
         "* %?\n%^T")
        ("h" "Hugo post" entry (file+olp "blog.org" "Blog")
         (function org-hugo-new-subtree-post-capture-template))
        ("s" "Shopping" plain (file +org-shopping-file)
         (function org-shopping-goto-last-open-or-make-new))))


(setq org-agenda-files (list +org-default-todo-file
                             +org-default-calendar-file
                             (f-join org-directory "lectures.org")))

(setq frame-title-format (list "%b - " (user-login-name) "@" (system-name)))

(defun sp-point-after-quote-p (id action context)
  "Pls."
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\"|'"))))

(after! smartparens
  ;; Auto-close more conservatively and expand braces on RET
  (show-smartparens-global-mode)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "\\[" "\\]")

  ;; Elixir stuff should work like python
  (sp-with-modes 'elixir-mode
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-python-fix-tripple-quotes))
    (sp-local-pair "\\'" "\\'")
    (sp-local-pair "\"\"\"" "\"\"\""))

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p
                       sp-point-after-quote-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))


(setq +doom-dashboard-pwd-policy 'last)

(use-package! py-isort
  :after python
  :config
  (map! :map python-mode-map
        :localleader
        :n "s" #'py-isort-buffer
        :v "s" #'py-isort-region))

(when ON-LAPTOP
  (after! disable-mouse
    (global-disable-mouse-mode)))

(after! ssh-deploy
  (ssh-deploy-add-after-save-hook)
  (setq ssh-deploy-on-explicit-save 1))

(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

;; stops the evil selection being added to the kill-ring
(fset 'evil-visual-update-x-selection 'ignore)

(setq projectile-require-project-root t)

(setq geiser-mode-start-repl-p t)

;; persist history
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist `(("." . ,(concat doom-emacs-dir "undo"))))

(setq posframe-mouse-banish nil)

(defun nuke-pretty-symbols (mode)
  (setq +pretty-code-symbols-alist
        (delq (assq mode +pretty-code-symbols-alist)
              +pretty-code-symbols-alist)))

(add-hook! python-mode
  (nuke-pretty-symbols 'python-mode)
  (set-pretty-symbols! 'python-mode
    :lambda "lambda"))

(add-hook! c-mode
  (nuke-pretty-symbols 'c-mode))

(add-hook! js-mode
  (nuke-pretty-symbols 'js-mode))

;; dunno if there's a better way to starting in paren mode
(add-hook! parinfer-mode
  (parinfer--switch-to-paren-mode))

(set-popup-rule! "^\\* Racket REPL"
  :side 'right
  :quit nil
  :size 0.35)

(set-popup-rule! "^\\*Man"
  :side 'right
  :size 0.35)

;; mhtml mode pls
(add-to-list 'auto-mode-alist '("\\.eex$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2$" . web-mode))

(setq treemacs-silent-refresh t
      treemacs-follow-mode t
      doom-themes-treemacs-theme "Default")

(add-hook 'after-make-frame-functions
          (lambda (_)
            (run-at-time "1 sec" nil #'doom/reload-theme)))

(use-package! slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config

  (slack-register-team
   :name "lucss"
   :default t
   :client-id (password-store-get "slack/lucss/id")
   :client-secret (password-store-get "slack/lucss/secret")
   :token (password-store-get "slack/lucss/token")
   :subscribed-channels '(general exec tech)
   :full-and-display-names t)

  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)

  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

(use-package! alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(setq deft-directory "~/org/lectures"
      deft-recursive t)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-github nil
      doom-modeline-major-mode-icon nil
      doom-modeline-icon t
      doom-modeline-enable-word-count t)

;; yeet

(setq +file-templates-alist (delq (assoc 'python-mode +file-templates-alist) +file-templates-alist))

(set-formatter! 'floskell "floskell" :modes '(haskell-mode))


(setq safe-local-variable-values '((ssh-deploy-async . 1)))
