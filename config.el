;;; private/ben/config.el -*- lexical-binding: t; -*-


;; bindings

(map!
 (:after neotree
   :map neotree-mode-map
   :n "|" #'neotree-enter-vertical-split
   :n "_" #'neotree-enter-horizontal-split)

 (:leader
   (:desc "file" :prefix "f"
     :desc "Neotree" :n "t" #'+neotree/open))

 (:map evil-window-map
   "<left>"     #'evil-window-left
   "<right>"    #'evil-window-right
   "<up>"       #'evil-window-up
   "<down>"     #'evil-window-down)

 (:map evil-motion-state-map
   "?" #'counsel-grep-or-swiper)

 "<home>" #'back-to-indentation-or-beginning
 "<end>" #'end-of-line)

(def-package! elpy)
(def-package! rainbow-identifiers)
(def-package! disable-mouse)
(def-package! clang-format)
(def-package! popup-kill-ring)
(def-package! elixir-yasnippets)
(def-package! transpose-frame)

(use-package pipenv
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)
  :hook (python-mode . pipenv-mode))

(def-package! flycheck-credo
  :commands flycheck-credo-setup
  :hook (elixir-mode . flycheck-credo-setup))

(setq ON-LAPTOP (string= (system-name) "laptop"))

(if ON-LAPTOP
    (progn)
    (progn
      (def-package! discord-emacs)
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
      (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
      (run-at-time "1 min" nil #'discord-emacs-run "384815451978334208")))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-quickhelp-mode 1
        company-quickhelp-delay 0.2)
  (global-company-mode)
  (set-company-backend!
    '(org-mode markdown-mode)
    '(company-files
      company-yasnippet
      company-dabbrev
      company-math-symbols-unicode)))

(after! rainbow-identifiers
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(setq doom-line-numbers-style nil)

(if ON-LAPTOP
    (setq doom-theme 'doom-tomorrow-night-eighties)
    (setq doom-theme 'doom-tomorrow-night))

; hip shit
(after! neotree
  (setq doom-neotree-file-icons t
        neo-theme 'icons))

(after! company-quickhelp
  (company-quickhelp-mode 1))

(after! elpy
  (setq elpy-syntax-check-command "epylint"
        elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv
                       elpy-module-yasnippet
                       elpy-module-sane-defaults))
  (elpy-enable))

(after! flycheck
;  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint
;                                            flycheck-pylintrc "~/.pylintrc")))
  (global-flycheck-mode))

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ditaa . t))))

(setq frame-title-format (list "%b - " (user-login-name) "@" (system-name)))

(add-hook! before-save #'delete-trailing-whitespace)

(after! smartparens
  ;; Auto-close more conservatively and expand braces on RET
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "\\[" "\\]")

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
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

(if ON-LAPTOP
  (after! disable-mouse
    (global-disable-mouse-mode)))


(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

(setq projectile-require-project-root t)
(fset 'evil-visual-update-x-selection 'ignore)

(setq geiser-mode-eval-last-sexp-to-buffer t
      geiser-mode-eval-to-buffer-prefix " ;=> "
      geiser-mode-start-repl-p t)


;; persisit history
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
