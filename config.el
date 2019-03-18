;;; private/ben/config.el -*- lexical-binding: t; -*-

;; bindings

(map!
 (:leader
   (:prefix "f"
     :desc "Toggle Treemacs" "t" #'+treemacs/toggle))

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

;; (def-package! elpy)
;; (def-package! rainbow-identifiers)
(def-package! disable-mouse)
(def-package! clang-format)
(def-package! popup-kill-ring)
(def-package! transpose-frame)
(def-package! evil-anzu)

(def-package! emojify
  :init
  (add-hook! 'after-init-hook #'global-emojify-mode))

(def-package! evil-lion
  :config
  (evil-lion-mode))

(def-package! drag-stuff
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(def-package! sqlup-mode
  :commands (sqlup-mode)
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

;; (use-package pipenv
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended)
;;   :hook (python-mode . pipenv-mode))

(def-package! anzu
  :config
  (global-anzu-mode +1))

(def-package! smart-hungry-delete
  ;; :ensure t
  :defer nil
  :config (smart-hungry-delete-add-default-hooks))

(def-package! backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(def-package! esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(setq ON-LAPTOP (string= (system-name) "laptop"))

(if ON-LAPTOP
    (progn)
  (progn
    (def-package! discord-emacs)
    (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
    (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
    (run-at-time "1 min" nil #'discord-emacs-run "384815451978334208")))

(after! lsp
  (setq lsp-enable-xref t
        lsp-enable-snippet t
        lsp-enable-completion-at-point nil
        lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))

  (set-formatter! 'lsp-formatter #'lsp-format-buffer
    :modes '(lsp-mode)))

(after! company-lsp
  (setq company-lsp-enable-snippet t))

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

(if ON-LAPTOP
    (setq doom-theme 'doom-tomorrow-night-eighties)
  (setq doom-theme 'doom-opera))

;; hip shit
;; (after! neotree
;;   (setq doom-neotree-file-icons t
;;         neo-theme 'icons))

;; (after! elpy
;;   (setq elpy-syntax-check-command "epylint"
;;         elpy-modules '(elpy-module-company
;;                        elpy-module-eldoc
;;                        elpy-module-pyvenv
;;                        elpy-module-yasnippet
;;                        elpy-module-sane-defaults))
;;   (elpy-enable))

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

  (setq org-attach-screenshot-command-line "escrotum -s %f")
  (setq org-reveal-root "~/dev/reveal.js"))

(setq org-log-done 'time
      +org-default-notes-file (f-join org-directory "notes.org")
      +org-default-todo-file (f-join org-directory "todo.org")
      +org-default-calendar-file (f-join org-directory "calendar.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline +org-default-todo-file "Inbox")
         "* [ ] %?\n%i" :prepend t :kill-buffer t)
        ("n" "Notes" entry (file+headline +org-default-notes-file "Inbox")
         "* %u %?\n%i" :prepend t :kill-buffer t)
        ("c" "Calendar" entry (file +org-default-calendar-file)
         "* %?\n%^T")))

(setq org-agenda-files (list +org-default-todo-file
                             +org-default-calendar-file
                             (f-join org-directory "lectures.org")))

(setq frame-title-format (list "%b - " (user-login-name) "@" (system-name)))

(add-hook! before-save #'delete-trailing-whitespace)

;; (defun sp-point-after-word-excepted (&rest words)
;;   "Return t if point is after a word, nil if otherwise or previous word is in the excepted list
;; This predicate is only tested on \"insert\" action."
;;   (let ((match-exp (format "(%s)\\Sw" (string-join words "|"))))
;;     (lambda (id action context)
;;       (when (eq action 'insert)
;;         (and (not (save-excursion
;;                     (backward-word)
;;                     (let ((r (looking-at match-exp)))
;;                       (message (format-message "matched? %s" r))
;;                       r)))
;;             (save-excursion
;;               (backward-char 1)
;;               (looking-back "\\sw\\|\\s_")))))))

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
  ;; ;; This lets us have f"" and b"" etc in python
  ;; (let ((unless-list `(sp-point-before-word-p
  ;;                      ,(sp-point-after-word-excepted "f" "r" "b")
  ;;                      sp-point-before-same-p)))
  ;;   (sp-local-pair 'python-mode "'" nil :unless unless-list)
  ;;   (sp-local-pair 'python-mode "\"" nil :unless unless-list))

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

(def-package! py-isort
  :after python
  :config
  (map! :map python-mode-map
        :localleader
        :n "s" #'py-isort-buffer
        :v "s" #'py-isort-region))

(after! evil-multiedit
  (evil-multiedit-default-keybinds))

(when ON-LAPTOP
  (after! disable-mouse
    (global-disable-mouse-mode)))


(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

;; stops the evil selection being added to the kill-ring
(fset 'evil-visual-update-x-selection 'ignore)

(setq projectile-require-project-root t
      projectile-enable-caching nil)

(setq geiser-mode-eval-last-sexp-to-buffer t
      geiser-mode-eval-to-buffer-prefix " ;=> "
      geiser-mode-start-repl-p t)


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

(after! treemacs
  (setq treemacs-silent-refresh t
        treemacs-follow-mode t
        doom-treemacs-use-generic-icons nil))

;; TODO: remove once treemacs is unborked
(require 'treemacs)

(add-hook 'after-make-frame-functions (lambda (frame)
                                        (doom/reload-font)))

(def-package! slack
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

(def-package! alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; (setq
;;  lsp-haskell-process-args-hie '("-d" "-l" "/tmp/hie.log" "+RTS" "-xc"))

(setq deft-directory "~/org/lectures"
      deft-recursive t)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-github nil
      doom-modeline-major-mode-icon nil
      doom-modeline-icon t
      doom-modeline-enable-word-count t)

(setq soundklaus-access-token (password-store-get "soundcloud/token"))
