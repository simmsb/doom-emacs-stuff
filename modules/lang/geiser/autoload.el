;;; lang/geiser/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +geiser/repl ()
  "Open the Gesier REPL."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "* Racket REPL *")
       (progn (racket-run-and-switch-to-repl)
              (let ((buf (get-buffer "* Racket REPL *")))
                (bury-buffer buf)
                buf)))))
