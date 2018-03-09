;;; private/ben/+bindings.el -*- lexical-binding: t; -*-


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

 "<home>" #'back-to-indentation-or-beginning
 "<end>" #'end-of-line
 "<backspace>" #'doom/backward-delete-whitespace-to-column)
