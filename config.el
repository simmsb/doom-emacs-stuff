;;; private/ben/config.el -*- lexical-binding: t; -*-

(load! +bindings)

(def-package! elpy)
(def-package! color-theme-sanityinc-tomorrow)
(def-package! rainbow-identifiers)
(def-package! disable-mouse)

(setq ON-LAPTOP (string= (system-name) "laptop"))

(if ON-LAPTOP
    (progn)
    (progn
      (def-package! discord-emacs)
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
      (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
      (run-at-time "1 min" nil #'discord-emacs-run "384815451978334208")
      (def-package! wakatime-mode)
      (after! wakatime-mode
        (global-wakatime-mode))))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-quickhelp-mode 1
        company-quickhelp-delay 0.2
        company-transformers '(company-sort-by-statistics))
  (global-company-mode)
  (set! :company-backend '(org-mode) '(company-files company-yasnippet company-dabbrev)))

(after! rainbow-identifiers
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(setq doom-line-numbers-style nil)
(setq doom-theme nil)

(if ON-LAPTOP
    (after! color-theme-sanityinc-tomorrow
      (color-theme-sanityinc-tomorrow-eighties))
  (after! color-theme-sanityinc-tomorrow
      (color-theme-sanityinc-tomorrow-night)))

; hip shit
(setq doom-neotree-file-icons t)

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
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint
                                            flycheck-pylintrc "~/.pylintrc")))
  (global-flycheck-mode))

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ditaa . t))))

;; There's something that borks my theme when loading a frame, so forcibly reload the theme
(add-hook! doom-init-ui
  (if ON-LAPTOP
      (load-theme 'sanityinc-tomorrow-eighties)
      (load-theme 'sanityinc-tomorrow-night)))

(setq frame-title-format (list "%b - " (user-login-name) "@" (system-name)))

(add-hook! prog-mode
  (doom|enable-delete-trailing-whitespace))

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

(add-hook! after-init #'clipmon-mode-start)
