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

;;;###autoload
(defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
         (fname (org-hugo-slug title))
         (date (format-time-string "%Y-%m-%d")))
    (mapconcat #'identity
               `(
                 ,(concat "* " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ,(concat ":EXPORT_DATE: " date)
                 ":END:"
                 "%?\n")          ;Place the cursor here finally
               "\n")))

;;;###autoload
(defun org-shopping-goto-last-open-or-make-new ()
  (with-current-buffer (org-capture-target-buffer (eval (nth 1 (org-capture-get :target))))
    (let ((last-shop
           (save-match-data
             (progn
                (goto-char (point-max))
                (if (search-backward-regexp (rx bol "*" (* space) (or "TODO" "DONE") (* space) "Shop" (* space) (group (+ digit))) nil 'noerror)
                    (string-to-number (match-string 1))
                  0)))))
      (save-match-data
        (progn
          (goto-char (point-max))
          (if (search-backward-regexp (rx bol "* TODO Shop" (* space) (+ digit)) nil 'noerror)
              (progn
                (goto-char (match-end 0))
                "- [ ] %?")
            (progn
              (goto-char (point-max))
              (concat "* TODO Shop " (number-to-string (+ last-shop 1)) "\n" "- [ ] %?"))))))))

;;;###autoload
(defun org-shopping-open ()
  (interactive)
  (find-file +org-shopping-file))

(defun org-notes-open ()
  (interactive)
  (find-file +org-default-notes-file))

;;;###autoload
(defun +file-templates/insert ()
  "Force insert a file template expansion"
  (interactive)
  (and buffer-file-name
       (not buffer-read-only)
       (bobp) (eobp)
       (not (member (substring (buffer-name) 0 1) '("*" " ")))
       (when-let (rule (cl-find-if #'+file-template-p +file-templates-alist))
         (apply #'+file-templates--expand rule))))

;;;###autoload
(defun +treemacs/toggle-project ()
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (if (doom-project-p)
           (treemacs-display-current-project-exclusively)
         (treemacs)))))
