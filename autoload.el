;;; autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
           (beginning-of-line)))

;;;###autoload
(defun position-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
for the current buffer's file name, and the line number at point."
  (interactive)
  (kill-new
   (format "%s:%d" (buffer-file-name) (save-restriction (widen) (line-number-at-pos)))))

;; uh so this shit is borked, copy over the folder for elixir-ls
;;;###autoload
(defun copy-elixir-ls-dirs ()
  "Copies elixir-ls dirs over from alchemist quelpa"
  (require 'package)
  (require 'f)
  (package-load-all-descriptors)

  (let ((elpa-alchemist-dir (f-join (package-desc-dir (car (alist-get 'alchemist package-alist))) "elixir-ls")))
    (when (f-exists? elpa-alchemist-dir)
      (f-delete elpa-alchemist-dir t))
    (f-copy
     (f-join doom-emacs-dir ".local" "packages" "quelpa" "build" "alchemist" "elixir-ls")
     elpa-alchemist-dir)))
