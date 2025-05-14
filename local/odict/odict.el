;;; odict.el --- Emacs interface for odict -*- lexical-binding: t; -*-

;; Copyright 2025 Ben Simms

;; Author: Ben Simms <ben@bensimms.moe>
;;
;; Maintainer: Ben Simms <ben@bensimms.moe>
;; Version: 0.0.0
;; Package-Requires ((emacs "27.1") (magit-section "20250501.848") (request "20250219.2213") (ht "20230703.558") (dash "20250312.1307") (s "20220902.1511"))

;;; Code:

(require 'cl-lib)
(require 's)
(require 'ht)
(require 'magit-section)
(require 'dash)
(require 'request)
(require 'jeison-odict)
(require 'visual-fill-column)

(defvar odict-buffer-name "*odict*")
(defvar odict-dictionaries nil)
(defvar odict-default-dictionary nil)
(defvar odict-program-path (executable-find "odict"))

(jeison-odict-defclass odict-pronunciation nil
  ((kind :initform nil) (value :type string)))

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

(defvar odict--sense-map (ht
                          ('abv "abbreviation")
                          ('adf "adfix")
                          ('art "article")
                          ('adj "adjective")
                          ('phr_adj "adjective phrase")
                          ('adv "adverb")
                          ('phr_adv "adverbial phrase")
                          ('aff "affix")
                          ('aux "auxiliary")
                          ('aux_adj "auxiliary adjective")
                          ('aux_v "auxiliary verb")
                          ('chr "character")
                          ('cf "circumfix")
                          ('cls "classifier")
                          ('conj "conjunction")
                          ('conj_c "coordinating conjunction")
                          ('contr "contraction")
                          ('cop "copula")
                          ('ctr "counter")
                          ('det "determiner")
                          ('expr "expression")
                          ('inf "infix")
                          ('intf "interfix")
                          ('intj "interjection")
                          ('vi "intransitive verb")
                          ('name "name")
                          ('n "noun")
                          ('num "numeric")
                          ('part "particle")
                          ('phr "phrase")
                          ('postp "postposition")
                          ('pref "prefix")
                          ('prep "preposition")
                          ('phr_prep "prepositional phrase")
                          ('pron "pronoun")
                          ('propn "proper noun")
                          ('prov "proverb")
                          ('punc "punctuation")
                          ('conj_s "subordinating conjunction")
                          ('suff "suffix")
                          ('sym "symbol")
                          ('vt "transitive verb")
                          ('un "unknown")
                          ('v "verb")))


(defun odict--insert-definition (defn &optional idx)
  (magit-insert-section (section-ordict-defn)
    (magit-insert-heading (concat
                           (propertize "Definition" 'face 'outline-1)
                           (unless (null idx)
                             (concat
                              " "
                              idx))
                           "\n"))
    (insert (format "%s\n" (oref defn value)))
    (-when-let (examples (oref defn examples))
      (cl-loop for exc in examples do
               (insert (propertize
                        (oref exc value)
                        'face
                        'org-macro))
               (-when-let (translations (oref exc translations))
                 (cl-loop for tr in translations do
                          (newline)
                          (insert (oref tr value))))
               (newline)))))

(defvar odict--special-definitions '("Position"
                                     "Article"
                                     "Etymology"
                                     "Group"
                                     "Typical with"
                                     "Synonyms"
                                     "Forms"))

(defun odict--handle-special-definitions (defs)
  (-let* (((special- normal) (--> defs
                                  (--separate (and
                                                (cl-typep it 'odict-definition-group)
                                                (member (oref it description) odict--special-definitions))
                                             it)))
          (special (-sort (-on (lambda (x y)
                                 (-let ((x-pos (-elem-index x odict--special-definitions))
                                        (y-pos (-elem-index y odict--special-definitions)))
                                   (pcase (list x-pos y-pos)
                                     (`(nil nil) (string< x y))
                                     (`(nil ,(pred (not null))) nil)
                                     (`(,(pred (not null)) nil) t)
                                     (`(,x-pos ,y-pos) (< x-pos y-pos)))))
                               (lambda (x) (oref x description))) special-))
          (found-specials (--map (oref it description) special)))
    (cl-loop for defn in special do
      (let* ((description (oref defn description)))
        (cond
         ((string= description "Position")
          (let* ((value (-> defn
                            (oref definitions)
                            (car)
                            (oref value))))
            (insert (propertize value 'face 'org-footnote))
            (unless (member "Article" found-specials)
              (newline))))
         ((string= description "Article")
          (let* ((value (-> defn
                            (oref definitions)
                            (car)
                            (oref value))))
            (insert " " (propertize value 'face 'org-date))
            (newline)))
         ((string= description "Etymology")
          (let* ((value (-> defn
                            (oref definitions)
                            (car)
                            (oref value))))
            (insert (propertize value 'face 'bold))
            (newline)))
         ((string= description "Group")
          (let* ((value (-> defn
                            (oref definitions)
                            (car)
                            (oref value))))
            (insert (propertize value 'face 'org-cite))))
         ((string= description "Typical with")
          (newline 2)
          (magit-insert-section (section-ordict-typical-with)
            (magit-insert-heading "Typical with\n")
            (let* ((value (--> defn
                            (oref it definitions)
                            (-map (lambda (x) (oref x value)) it)
                            (s-join ", " it)
                            (s-trim it))))
              (insert (propertize value 'face 'org-footnote)))))
         ((string= description "Synonyms")
          (newline 2)
          (magit-insert-section (section-ordict-typical-with)
            (magit-insert-heading "Synonyms\n")
            (let* ((value (--> defn
                            (oref it definitions)
                            (-map (lambda (x) (oref x value)) it)
                            (s-join ", " it)
                            (s-trim it))))
              (insert (propertize value 'face 'org-footnote)))))
         ((string= description "Forms")
          (newline 2)
          (magit-insert-section (section-ordict-forms)
            (magit-insert-heading "Forms\n")
            (cl-loop for subdef in (oref defn definitions) do
                (insert (propertize (oref subdef value) 'face 'org-footnote))
                (newline)))))))
    normal))


(defun odict--insert-definition-group (defn &optional idx)
  (magit-insert-section (section-ordict-defn-group)
    (magit-insert-heading (concat
                           (propertize (concat
                                         (propertize "Definition group" 'face 'outline-2)
                                         (unless (null idx)
                                           (concat
                                            " "
                                            (number-to-string idx))))
                                       'display '(height 1.05))
                           "\n"))
    (unless (string-empty-p (oref defn description))
      (insert (format "%s\n" (oref defn description))))
    (-when-let (subdefs (oref defn definitions))
      (cl-loop for idx from 1 for subdef in subdefs do
               (odict--insert-definition subdef (org-number-to-letters idx))
               (newline)))))

(defun odict--handle-sense (sense)
  (-when-let (lemma (oref sense lemma))
    (unless (s-blank-str? lemma)
      (insert (format "%s" lemma))
      (newline)))
  (-let [defs (odict--handle-special-definitions (oref sense definitions))]
    (newline 2)
    (cl-loop for idx from 1 for defn in defs do
             (when (cl-typep defn 'odict-definition)
               (odict--insert-definition defn (number-to-string idx))
               (newline))
             (when (cl-typep defn 'odict-definition-group)
               (newline)
               (odict--insert-definition-group defn idx))))
  (-when-let (tags (oref sense tags))
    (insert (s-join ", " tags)))
  (-when-let (forms (oref sense forms))
    (magit-insert-section (section-ordict-forms)
      (magit-insert-heading "Forms\n")
      (insert (s-join ", " (-map (lambda (form) (oref form term)) forms)))))
  (-when-let (translations (oref sense translations))
    (magit-insert-section (section-ordict-translations)
      (magit-insert-heading "Translations\n")
      (cl-loop for translation in translations do
               (insert (format "%s: %s" (oref translation lang) (oref translation value)))))))


(defun odict--handle-results (word string-resp)
  "Handle odict lookup results"
  (let* ((entries (jeison-odict-read '(list-of odict-entry) string-resp)))
    (with-odict-buffer
      (setq-local visual-line-fringe-indicators '(t nil)
                  fill-column 120
                  visual-fill-column-center-text t)
      (visual-line-fill-column-mode t)
      (insert (concat
                (propertize
                  (format "Results for %s" word)
                  'face
                  'info-title-1)
                "\n"))
      (cl-loop for entry in entries do
               (magit-insert-section (section-odict-entry)
                 (magit-insert-heading (format "%s\n" (oref entry term)))
                 (-when-let (see-also (oref entry see-also))
                   (insert (format "See also: %s\n\n" see-also)))
                 (cl-loop for idx from 1 for etymology in (oref entry etymologies) do
                          (magit-insert-section (section-odict-etymology)
                            (magit-insert-heading (concat (propertize
                                                           (concat
                                                            (propertize "Etymology" 'face 'outline-3)
                                                            " "
                                                            (number-to-string idx))
                                                           'display '(height 1.1))
                                                          "\n"))
                            (unless (null (oref etymology pronunciations))
                              (insert (format "Pronunciations: %s\n"
                                              (s-join ", " (-map (lambda (p) (oref p value))
                                                                 (oref etymology pronunciations))))))
                            (-when-let (description (oref etymology description))
                              (unless (s-blank-str? description)
                                (insert (format "%s\n" description))))
                            (if (length= (oref etymology senses) 1)
                                (progn
                                  (insert (concat (ht-get odict--sense-map (caar (oref etymology senses)))
                                                  "\n"))
                                  (odict--handle-sense (cdar (oref etymology senses))))
                              (cl-loop for (k . v) in (oref etymology senses) do
                                         (magit-insert-section (section-odict-sense)
                                           (magit-insert-heading (format "As %s\n" (ht-get odict--sense-map k)))
                                           (odict--handle-sense v))))))))
      (goto-char (point-min)))))

(defconst odict-process-name " %odict-mode-process%")
(defconst odict-process-buffer-name " *odict-mode-process*")
(defvar odict--process-kill-timer nil)

(defun odict-stop-process ()
  "Kill the odict server if it is running."
  (let ((process (get-process odict-process-name)))
    (when (and process (> (process-id process) 0))
      (kill-process process))))

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
    (unless (null odict--process-kill-timer)
      (cancel-timer odict--process-kill-timer))
    (setq odict--process-kill-timer (run-at-time "20 min" nil #'odict-stop-process))
    process))

(defun odict--handle-failure (process status pfuture-buffer)
  "Handle odict lookup failure"
  (message "odict lookup failed? %s" pfuture-status))

(defun odict-lookup (word &optional dictionary)
  "Lookup WORD with odict"
  (interactive
   (list (or (if (equal major-mode 'pdf-view-mode)
                 (car (pdf-view-active-region-text)))
             (doom-thing-at-point-or-region 'word)
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

(defun odict-search (word &optional dictionary)
  "Lookup WORD with odict"
  (interactive
   (list (or (if (equal major-mode 'pdf-view-mode)
                 (car (pdf-view-active-region-text)))
             (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         odict-default-dictionary))
  (let ((dictionary- (or dictionary odict-default-dictionary))
        (word- (s-downcase word)))
    (odict-get-process)
    (request (format "http://localhost:5005/%s/search" dictionary-)
      :params `(("query" . ,word-))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (odict--handle-results word data)))))
  nil)

(provide 'odict)
;;; odict.el ends here
