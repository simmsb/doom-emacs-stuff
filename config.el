;;; private/ben/config.el -*- lexical-binding: t; -*-

(load! +bindings)

(setq doom-font (font-spec :family "Fira Mono"
                           :size (if (string= (system-name) "home") 19 12)) ; size 19 on pc, 12 on laptop
      doom-unicode-font (font-spec :family "Fira Mono"))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-quickhelp-mode 1
        company-quickhelp-delay 0.0)
  (global-company-mode))

(after! rainbow-identifiers
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(setq doom-line-numbers-style nil)
(setq doom-theme nil)

;;(set! :company-backend 'python-mode '(company-yasnippet))

(after! color-theme-sanityinc-tomorrow
  (color-theme-sanityinc-tomorrow-night))

; hip shit
(setq doom-neotree-file-icons t)

(after! mode-icons
  (mode-icons-mode))

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
                                            flycheck-pylintrc "~/.pylintrc"))))
(after! whitespace-cleanup-mode
  (global-whitespace-cleanup-mode))

(after! org
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ditaa . t))))

;; There's something that borks my theme when loading a frame, so forcibly reload the theme
(add-hook 'doom-init-ui-hook #'(lambda () (load-theme 'sanityinc-tomorrow-night)))
