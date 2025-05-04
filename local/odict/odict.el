;;; odict.el --- Emacs interface for odict -*- lexical-binding: t; -*-

;; Copyright 2025 Ben Simms

;; Author: Ben Simms <ben@bensimms.moe>
;;
;; Maintainer: Ben Simms <ben@bensimms.moe>
;; Version: 0.0.0
;; Package-Requires ((emacs "27.1") (magit-section "20250501.848") (request "20250219.2213"))

;;; Code:

(require 'cl-lib)
(require 's)
(require 'magit-section)
(require 'dash)
(require 'request)
(require 'jeison-odict)

(defvar odict-buffer-name "*odict*")
(defvar odict-dictionaries nil)
(defvar odict-default-dictionary nil)
(defvar odict-program-path (executable-find "odict"))

(jeison-odict-defclass odict-pronunciation nil
  ((kind :type string) (value :type string)))

(jeison-odict-defclass odict-translation nil
  ((lang :type string) (value :type string)))

(jeison-odict-defclass odict-example nil
  ((value :type string)
   (translations :type (list-of odict-translation))
   (pronunciations :type (list-of odict-pronunciation))))

(jeison-odict-defclass odict-note nil
  ((id :initform nil)
   (value :type string)
   (examples :type (list-of odict-example))))

(jeison-odict-defclass odict-definition nil
  ((id :initform nil)
   (value :type string)
   (examples :type (list-of odict-example))
   (notes :type (list-of odict-note))))

(jeison-odict-defclass odict-definition-group nil
  ((id :initform nil)
   (description :type string)
   (definitions :type (list-of odict-definition))))

(cl-deftype tagged-by (tag cases)
  t)

(cl-deftype alist-of (elem-typ)
  t)

(jeison-odict-defclass odict-form nil
  ((kind :initform nil)
   (term :type string)
   (tags :type (list-of string))))

(jeison-odict-defclass odict-sense nil
  ((pos :type string)
   (lemma :initform nil)
   (definitions :type (list-of (tagged-by type ((group . odict-definition-group)
                                                (definition . odict-definition)
                                                (nil . odict-definition)))))
   (tags :type (list-of string))
   (translations (list-of odict-translation))
   (forms :type (list-of odict-form))))

(jeison-odict-defclass odict-etymology nil
  ((id :initform nil)
   (pronunciations :type (list-of odict-pronunciation))
   (description :initform nil)
   (senses :type (alist-of odict-sense))))

(jeison-odict-defclass odict-entry nil
  ((term :type string)
   (see-also :path see_also :initform nil)
   (etymologies :type (list-of odict-etymology))))

(defmacro with-odict-buffer (&rest body)
  `(let ((buffer (get-buffer-create odict-buffer-name)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (magit-section-mode)
         (magit-insert-section (odict-buffer)
           ,@body)))
     (switch-to-buffer-other-window buffer)))

(defun odict--insert-definition (defn)
  (magit-insert-section (section-ordict-defn)
    (magit-insert-heading "Definition:")
    (insert (format "%s\n" (oref defn value)))
    (-when-let (examples (oref defn examples))
      (cl-loop for exc in examples do
               (insert (propertize
                        (oref exc value)
                        'face
                        'org-macro))
               (newline)))))

(defun odict--insert-definition-group (defn)
  (let* ((description (oref defn description)))
    (cond
     ((string= description "Position")
      (let* ((value (-> defn
                        (oref definitions)
                        (car)
                        (oref value))))
        (insert (propertize value 'face 'org-code))
        (newline)))
     ((string= description "Etymology")
      (let* ((value (-> defn
                        (oref definitions)
                        (car)
                        (oref value))))
        (insert (propertize value 'face 'bold))
        (newline)))
     ((string= description "Typical with")
      (magit-insert-section (section-ordict-typical-with)
        (magit-insert-heading "Typical with\n")
        (let* ((value (--> defn
                        (oref it definitions)
                        (-map (lambda (x) (oref x value)) it)
                        (s-join ", " it))))
          (insert (propertize value 'face 'org-code))
          (newline))))
     ((string= description "Forms")
      (magit-insert-section (section-ordict-forms)
        (magit-insert-heading "Forms\n")
        (cl-loop for subdef in (oref defn definitions) do
            (insert (propertize (oref subdef value) 'face 'org-code)))
        (newline)))
     ((string= description "Group")
      (let* ((value (-> defn
                        (oref definitions)
                        (car)
                        (oref value))))
        (insert (propertize value 'face 'org-code))
        (newline)))
     (t (magit-insert-section (section-ordict-defn-group)
          (magit-insert-heading "Definition:")
          (insert (format "%s\n" (oref defn description)))
          (-when-let (subdefs (oref defn definitions))
            (cl-loop for subdef in subdefs do
                     (odict--insert-definition subdef))))))))

(defun odict--handle-results (word string-resp)
  "Handle odict lookup results"
  (let* ((entries (jeison-odict-read '(list-of odict-entry) string-resp)))
    (with-odict-buffer
      (insert (format "Results for %s\n" word))
      (cl-loop for entry in entries do
               (magit-insert-section (section-odict-entry)
                 (magit-insert-heading (format "%s\n" (oref entry term)))
                 (-when-let (see-also (oref entry see-also))
                   (insert (format "See also: %s\n\n" see-also)))
                 (cl-loop for etymology in (oref entry etymologies) do
                          (magit-insert-section (section-odict-etymology)
                            (magit-insert-heading t "Etymology:")
                            (insert (format "Pronunciations: %s\n"
                                            (s-join ", " (-map (lambda (p) (oref p value))
                                                               (oref etymology pronunciations)))))
                            (-when-let (description (oref etymology description))
                              (insert (format "%s\n" description)))
                            (newline)
                            (cl-loop for (k . v) in (oref etymology senses) do
                                     (magit-insert-section (section-odict-sense)
                                       ;; TODO: correct name for sense
                                       (magit-insert-heading (format "%s\n" k))
                                       (-when-let (lemma (oref v lemma))
                                         (insert (format "%s" lemma)))
                                       (cl-loop for defn in (oref v definitions) do
                                                (when (cl-typep defn odict-definition)
                                                  (odict--insert-definition defn))
                                                (when (cl-typep defn odict-definition-group)
                                                  (odict--insert-definition-group defn)))
                                       (-when-let (tags (oref v tags))
                                         (insert (s-join ", " tags)))
                                       (-when-let (forms (oref v forms))
                                         (magit-insert-section (section-ordict-forms)
                                           (magit-insert-heading "Forms\n")
                                           (insert (s-join ", " (-map (lambda (form) (oref form term)) forms)))))
                                       (-when-let (translations (oref v translations))
                                         (magit-insert-section (section-ordict-translations)
                                           (magit-insert-heading "Translations\n")
                                           (cl-loop for translation in translations do
                                                    (insert (format "%s: %s" (oref translation lang) (oref translation value)))))))))))))))


(defconst odict-process-name " %odict-mode-process%")
(defconst odict-process-buffer-name " *odict-mode-process*")

(defun odict-get-process ()
  "Get or create the odict process."
  (let ((process (get-process odict-process-name)))
    (unless (and process (> (process-id process) 0))
      (with-current-buffer (get-buffer-create
                            odict-process-buffer-name)
        (erase-buffer)
        (setq process (apply #'start-process
                             odict-process-name
                             odict-process-buffer-name
                             odict-program-path
                             `("serve" ,@odict-dictionaries)))
        (set-process-query-on-exit-flag process nil)))
    process))

(defun odict--handle-failure (process status pfuture-buffer)
  "Handle odict lookup failure"
  (message "odict lookup failed? %s" pfuture-status))

(defun odict-lookup (word &optional dictionary)
  "Lookup WORD with odict"
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (if (equal major-mode 'pdf-view-mode)
                 (car (pdf-view-active-region-text)))
             (read-string "Look up in dictionary: "))
         odict-default-dictionary))
  (let ((dictionary- (or dictionary odict-default-dictionary))
        (word- (s-downcase word)))
    (odict-get-process)
    (request (format "http://localhost:5005/%s/lookup" dictionary-)
      :params `(("queries" . ,word-))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (odict--handle-results word data)))))
  nil)

(provide 'odict)
;;; odict.el ends here
