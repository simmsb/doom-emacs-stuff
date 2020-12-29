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
 ;; (:mode (emacs-lisp-mode clojure-mode scheme-mode lisp-mode)
 ;;  :i "<backspace>" #'parinfer-backward-delete-char
 ;;  :i "C-d" #'delete-char)

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
  ;; (ht-set! (lsp--client-notification-handlers (gethash 'hie lsp-clients)) "$/progress" #'ignore)
  ;; (lsp-haskell-set-formatter "ormolu")
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))


(use-package! org-ref
  :config
  (setq bibtex-completion-bibliography '("~/org/bibliography/references.bib")
        bibtex-completion-library-path '("~/org/research_stuff")
        bibtex-completion-notes-path "~/org/bibliography/notes.org"))

(use-package! screenshot
  :commands (screenshot)
  :config/el-patch
  (defun screenshot--post-process (file)
    "Apply any image post-processing to FILE."
    (when (or (> screenshot-radius 0)
              (> screenshot-shadow-radius 0))
      (let ((result
             (shell-command-to-string
              (format (el-patch-concat "convert '%1$s' \\( +clone -alpha extract \\
\\( -size %2$dx%2$d xc:black -draw 'fill white circle %2$d,%2$d %2$d,0' -write mpr:arc +delete \\) \\
\\( mpr:arc \\) -gravity northwest -composite \\
\\( mpr:arc -flip \\) -gravity southwest -composite \\
\\( mpr:arc -flop \\) -gravity northeast -composite \\
\\( mpr:arc -rotate 180 \\) -gravity southeast -composite \\) \\
-alpha off -compose CopyOpacity -composite -compose over \\
\\( +clone -background '%3$s' -shadow %4$dx%5$d+%6$d+%7$d \\) \\
+swap -background none -layers merge "
                                       (el-patch-add "-matte -virtual-pixel transparent -distort perspective-projection '0.931507, -0.0205634, 5, -6.79666e-16, 0.89108, 10, -8.06666e-18, -0.00028169' ")
                                       "'%1$s'")
                      file
                      screenshot-radius
                      screenshot-shadow-color
                      screenshot-shadow-intensity
                      screenshot-shadow-radius
                      screenshot-shadow-offset-horizontal
                      screenshot-shadow-offset-vertical))))
        (unless (string= result "")
          (error "Could not apply imagemagick commants to image:\n%s" result))))
    (run-hook-with-args 'screenshot-post-process-hook file))
  :config
  (setq screenshot-shadow-color "rgba(0, 0, 0, 0.55)"
        screenshot-shadow-radius 20
        screenshot-shadow-intensity 50
        screenshot-shadow-offset-horizontal 0
        screenshot-shadow-offset-vertical 20
        screenshot-border-width 5))


;; (when ON-DESKTOP
;;   (use-package! mu4e-alert
;;     :config (mu4e-alert-set-default-style (if ON-LAPTOP 'notifier 'libnotify))
;;     :hook ((after-init . mu4e-alert-enable-notifications)
;;            (after-init . mu4e-alert-enable-mode-line-display)))

;;   (use-package! mu4e
;;     :config
;;     (setq mu4e-update-interval (* 60 5))
;;     (set-email-account! "gmail.com"
;;                         `((mu4e-sent-folder . "/gmail.com/Sent Mail")
;;                           (mu4e-drafts-folder . "/gmail.com/Drafts")
;;                           (mu4e-trash-folder . "/gmail.com/Bin")
;;                           (mu4e-refile-folder . "/gmai.com/All Mail")
;;                           (smtpmail-smtp-user . ,user-mail-address)))))

(when ON-DESKTOP
  (use-package! discord-emacs)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
  (run-at-time "1 min" nil #'discord-emacs-run "384815451978334208"))

(after! lsp
  (lsp-ui-mode +1)
  (setq lsp-flycheck-live-reporting +1
        lsp-lens-enable nil
        lsp-modeline-diagnostics-scope :project
        lsp-enable-indentation t
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil
        lsp-enable-semantic-highlighting nil)
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

(when ON-LAPTOP
  (after! disable-mouse
    (global-disable-mouse-mode)))

(after! haskell-mode
  (setq haskell-auto-insert-module-format-string "-- | \nmodule %s\n    (\n     ) where"))

(after! evil
  (setq evil-normal-state-cursor '(box "light blue")
        evil-insert-state-cursor '(bar "medium sea green")
        evil-visual-state-cursor '(hollow "orange")
        evil-want-fine-undo t)
  (setq evil-vsplit-window-right t
        evil-split-window-below t)
  ;; stops the evil selection being added to the kill-ring
  (fset 'evil-visual-update-x-selection 'ignore)
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (+ivy/switch-buffer))
  (advice-add 'evil-ex-search-next :after
              (lambda (&rest _x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-ex-search-previous :after
              (lambda (&rest _x) (evil-scroll-line-to-center (line-number-at-pos)))))

(after! ivy
  (add-to-list 'ivy-re-builders-alist `(counsel-find-file . ,#'+ivy-prescient-non-fuzzy))
  (add-to-list 'ivy-re-builders-alist `(counsel-file-jump . ,#'+ivy-prescient-non-fuzzy))
  (add-to-list 'ivy-re-builders-alist `(counsel-projectile-find-file . ,#'+ivy-prescient-non-fuzzy)))

(setq-default x-stretch-cursor t
              uniquify-buffer-name-style 'forward)

(setq projectile-require-project-root t)

(setq posframe-mouse-banish nil)

(setq display-line-numbers-type nil)

(global-subword-mode 1)

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

(setq +treemacs-git-mode 'deferred)

(after! treemacs
  (treemacs-follow-mode +1)
  (setq treemacs-silent-refresh t
        doom-themes-treemacs-theme "Default"
        doom-themes-treemacs-bitmap-indicator-width 7))

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

(setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s 2>/dev/null || true")
