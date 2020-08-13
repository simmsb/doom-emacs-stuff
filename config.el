;;; private/ben/config.el -*- lexical-binding: t; -*-

(setq ON-DESKTOP (string= (system-name) "home"))
(setq ON-LAPTOP (string= (system-name) "laptop"))

;; bindings
(map!
 (:leader
  (:prefix "f"
   :desc "Toggle Treemacs" "t" #'+treemacs/toggle)
  (:prefix "o"
   :desc "Open Shopping" "s" #'org-shopping-open
   :desc "Open kill ring" "k" #'+default/yank-pop))

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
 (:mode (emacs-lisp-mode clojure-mode scheme-mode lisp-mode)
  :i "<backspace>" #'parinfer-backward-delete-char
  :i "C-d" #'delete-char)

 "<home>" #'back-to-indentation-or-beginning
 "<end>" #'end-of-line)

(use-package! disable-mouse)
(use-package! github-review)
(use-package! github-browse-file)

(use-package! geros
  :config
  (setq geros-eval-result-duration nil)
  :hook
  (geiser-mode . geros-mode))

(use-package! evil-lion
  :config
  (evil-lion-mode))

(use-package! sqlup-mode
  :commands (sqlup-mode)
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(use-package! backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package! esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package! lsp-haskell
  :config
  ;; progress spams the minibuffer when we're viewing hovers, etc
  (ht-set! (lsp--client-notification-handlers (gethash 'hie lsp-clients)) "$/progress" #'ignore)
  (lsp-haskell-set-formatter "ormolu")
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))


(when ON-DESKTOP
  (use-package! mu4e-alert
    :config (mu4e-alert-set-default-style (if ON-LAPTOP 'notifier 'libnotify))
    :hook ((after-init . mu4e-alert-enable-notifications)
           (after-init . mu4e-alert-enable-mode-line-display)))

  (use-package! mu4e
    :config
    (setq mu4e-update-interval (* 60 5))
    (set-email-account! "gmail.com"
                        `((mu4e-sent-folder . "/gmail.com/Sent Mail")
                          (mu4e-drafts-folder . "/gmail.com/Drafts")
                          (mu4e-trash-folder . "/gmail.com/Bin")
                          (mu4e-refile-folder . "/gmai.com/All Mail")
                          (smtpmail-smtp-user . ,user-mail-address)))))

(when ON-DESKTOP
  (use-package! discord-emacs)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  (run-at-time "1 min" nil #'discord-emacs-run "384815451978334208"))

(after! lsp
  (lsp-ui-mode +1)
  (setq lsp-flycheck-live-reporting +1
        lsp-modeline-diagnostics-scope :project
        lsp-enable-file-watchers t
        lsp-enable-text-document-color t
        lsp-enable-semantic-highlighting t)
  (add-to-list 'lsp-language-id-configuration '(cuda-mode . "ccls"))
  (add-to-list 'lsp-language-id-configuration '(p4lang-mode . "p4")))

(after! magit
  (magit-wip-mode 1))

(after! flycheck
  (add-hook! haskell-mode
    (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
    (add-to-list 'flycheck-disabled-checkers 'haskell-hlint)))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(after! lsp-rust
  (setq lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-server-display-inlay-hints t))

(after! company
  (setq company-idle-delay 0.1)
  (add-hook! evil-normal-state-entry #'company-abort)
  (set-company-backend! '(text-mode
                          markdown-mode
                          gfm-mode)
    '(:seperate
      company-ispell
      company-files
      company-yasnippet)))

(add-hook! prog-mode #'rainbow-delimiters-mode)

(after! org
  (setq org-tags-column 100)
  (setq org-sticky-header-full-path 'full)

  (add-hook! org-mode
    (org-sticky-header-mode 1))

  (add-to-list 'org-src-lang-modes '("rust" . rustic))

  (setq org-attach-screenshot-command-line "escrotum -s %f")
  (setq org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0)

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
                               (f-join org-directory "lectures.org"))))

(setq frame-title-format (list "%b - " (user-login-name) "@" (system-name)))

(after! smartparens
  (show-smartparens-global-mode))

(when ON-LAPTOP
  (after! disable-mouse
    (global-disable-mouse-mode)))

(after! ssh-deploy
  (ssh-deploy-add-after-save-hook)
  (setq ssh-deploy-on-explicit-save 1))

(after! haskell-mode
  (setq haskell-auto-insert-module-format-string "-- | \nmodule %s\n    (\n     ) where"))

(setq-default x-stretch-cursor t
              uniquify-buffer-name-style 'forward)

(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange")
      evil-want-fine-undo t)

;; stops the evil selection being added to the kill-ring
(fset 'evil-visual-update-x-selection 'ignore)

(setq projectile-require-project-root t)

(setq posframe-mouse-banish nil)

(setq display-line-numbers-type nil)

(global-subword-mode +1)

(defun nuke-pretty-symbols (mode)
  (setq +pretty-code-symbols-alist
        (delq (assq mode +pretty-code-symbols-alist)
              +pretty-code-symbols-alist)))

(add-hook! python-mode
  (nuke-pretty-symbols 'python-mode)
  (set-pretty-symbols! 'python-mode
    :lambda "lambda"))

(add-hook! c-mode
  (nuke-pretty-symbols 'c-mode)
  (nuke-pretty-symbols 'c++-mode))

(add-hook! c++-mode
  (nuke-pretty-symbols 'c++-mode)
  (nuke-pretty-symbols 'c-mode))

(add-hook! js-mode
  (nuke-pretty-symbols 'js-mode))

(add-hook! typescript-mode
  (nuke-pretty-symbols 'typescript-mode))

(add-hook! elixir-mode
  (nuke-pretty-symbols 'elixir-mode))

(add-hook! web-mode
  (nuke-pretty-symbols 'web-mode))

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

;; (add-hook 'after-make-frame-functions
;;           (lambda (_)
;;             (run-at-time "1 sec" nil #'doom/reload-theme)))

(setq deft-directory "~/org/lectures"
      deft-recursive t)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-github nil
      doom-modeline-major-mode-icon nil
      doom-modeline-icon t
      doom-modeline-enable-word-count t)

;; yeet
(set-formatter! 'ormolu "ormolu" :modes '(haskell-mode))

(setq safe-local-variable-values '((ssh-deploy-async . 1)))

(add-hook! cuda-mode
  (yas-minor-mode-on))

(set-irc-server! "chat.freenode.net"
                 `(:tls t :port 6697 :nick "nitros_" :sasl-username "nitros_" :sasl-password (lambda (&rest _) (password-store-get "freenode/pass")) :channels ("#emacs" "#haskell")))

(after! circe
  (enable-circe-color-nicks)
  (enable-circe-notifications))

(after! forge
  (if (atom forge-topic-list-limit)
      (setq forge-topic-list-limit (cons forge-topic-list-limit -5))
    (setcdr forge-topic-list-limit -5)))

(defun cc-bytecomp-is-compiling (&rest _))
