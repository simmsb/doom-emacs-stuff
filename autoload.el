;;; autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
           (beginning-of-line)))
