;;; private/ben/config.el -*- lexical-binding: t; -*-

(load! +bindings)

(def-package! elpy)
(def-package! color-theme-sanityinc-tomorrow)
(def-package! rainbow-identifiers)
(def-package! discord-ipc)
(def-package! whitespace-cleanup-mode)

(after! whitespace-cleanup-mode
  (global-whitespace-cleanup-mode))

(if (string-equal (system-name)
                  "home")
    (run-at-time "1 min" nil #'discord-ipc-run "384815451978334208"))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-quickhelp-mode 1
        company-quickhelp-delay 0.0
        company-transformers '(company-sort-by-statistics))
  (global-company-mode)
  (set! :company-backend '(org-mode) '(company-math-symbols-unicode
                                       company-files
                                       company-yasnippet
                                       company-dabbrev)))

(after! rainbow-identifiers
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(setq doom-line-numbers-style nil)
(setq doom-theme nil)

;;(set! :company-backend 'python-mode '(company-yasnippet))

(after! color-theme-sanityinc-tomorrow
  (color-theme-sanityinc-tomorrow-night))

(after! yasnippet
  (setq yas-snippet-dirs
        (append (list (expand-file-name "snippets/" (file-name-directory load-file-name)))
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

; hip shit
(setq doom-neotree-file-icons t)

(after! company-quickhelp
  (company-quickhelp-mode 1))

(after! elpy
  (setq elpy-syntax-check-command "epylint"
        elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv
                       elpy-module-flymake
                       elpy-module-yasnippet
                       elpy-module-sane-defaults))
  (elpy-enable))

(after! flycheck
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint
                                            flycheck-pylintrc "~/.pylintrc")))
  (global-flycheck-mode))

(after! org
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ditaa . t))))

;; There's something that borks my theme when loading a frame, so forcibly reload the theme
(add-hook! doom-init-ui (load-theme 'sanityinc-tomorrow-night))

(add-hook! prog-mode (setq-local show-trailing-whitespace t))

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

(def-package! magithub
  :after magit
  :ensure t
  :config
  (magithub-feature-autoinject t))
