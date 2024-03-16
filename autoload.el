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
(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the
  background.

 Return the temporary output buffer which command is writing to
 during execution.

 When the command is finished, call CALLBACK with the resulting
 output as a string."
  (let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

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


;;;###autoload
(defun forge-create-branch-for-issue (issue start-point &optional name)
  "Create a branch for an ISSUE starting at START-POINT with the branch name NAME."
  (interactive (list (forge-read-open-issue "Issue")
                     (magit-read-starting-point "Create issue branch")))
  (let* ((issue (forge-get-issue issue))
         (name (or name
                   (format "%s/%s-%s"
                           (ghub--username (ghub--host nil))
                           (oref issue number)
                           (org-hugo-slug (oref issue title))))))
    (magit-branch-and-checkout name start-point)
    (message "Created branch %s starting at %s" name start-point)))

;;;###autoload
(defun +corfu--enable-in-minibuffer ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (modulep! :completion helm)
                   (helm--alive-p))
              (corfu-mode +1))))


;;;###autoload
(defun lsp-rust-analyzer-view-mir ()
  "View Mir of function at point."
  (interactive)
  (-let* ((params (lsp-make-rust-analyzer-expand-macro-params
                   :text-document (lsp--text-document-identifier)
                   :position (lsp--cur-position)))
          (results (lsp-send-request (lsp-make-request
                                      "rust-analyzer/viewMir"
                                      params))))
    (let ((buf (get-buffer-create "*rust-analyzer mir*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (special-mode)
        (erase-buffer)
        (insert results)
        (pop-to-buffer buf)))))

;;;###autoload
(defun lsp-rust-analyzer-view-memory-layout ()
  "View memory layout of function at point."
  (interactive)
  (-let* ((params (lsp-make-rust-analyzer-expand-macro-params
                   :text-document (lsp--text-document-identifier)
                   :position (lsp--cur-position)))
          (results (lsp-send-request (lsp-make-request
                                      "rust-analyzer/viewRecursiveMemoryLayout"
                                      params))))
    (let ((buf (get-buffer-create "*rust-analyzer memory layout*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (special-mode)
        (erase-buffer)
        (insert (pp-to-string results))
        (pop-to-buffer buf)))))
