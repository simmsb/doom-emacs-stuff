;;; jeison-odict.el --- A library for declarative JSON parsing -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Valeriy Savchenko (GNU/GPL Licence)

;; Authors: Valeriy Savchenko <sinmipt@gmail.com>
;; URL: http://github.com/SavchenkoValeriy/jeison-odict
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (dash "2.16.0"))
;; Keywords: lisp json data-types

;; This file is NOT part of GNU Emacs.

;; jeison-odict is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jeison-odict is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with jeison-odict.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Jeison-Odict is a library for transforming JSON objects (or `alist's) into
;; EIEIO objects.

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 'eieio)

(defmacro jeison-odict-defclass (name superclasses slots &rest options-and-doc)
  "Define NAME as a new class derived from SUPERCLASS with SLOTS.

This macro is fully compatible with `defclass' macro and accepts the same
arguments.  The only difference is additional `:path' tag for slots.

OPTIONS-AND-DOC is used as the class' options and base documentation.
SUPERCLASSES is a list of superclasses to inherit from, with SLOTS
being the slots residing in that class definition.  Supported tags are:

  :initform   - Initializing form.
  :initarg    - Tag used during initialization.
  :accessor   - Tag used to create a function to access this slot.
  :allocation - Specify where the value is stored.
                Defaults to `:instance', but could also be `:class'.
  :writer     - A function symbol which will `write' an object's slot.
  :reader     - A function symbol which will `read' an object.
  :type       - The type of data allowed in this slot (see `typep').
  :documentation
              - A string documenting use of this slot.
  :path       - A path in JSON that is used to find the value for this slot
                during the `jeison-odict-read' parsing process.

The following are extensions on CLOS:
  :custom     - When customizing an object, the custom :type.  Public only.
  :label      - A text string label used for a slot when customizing.
  :group      - Name of a customization group this slot belongs in.
  :printer    - A function to call to print the value of a slot.
                See `eieio-override-prin1' as an example.

A class can also have optional options.  These options happen in place
of documentation (including a :documentation tag), in addition to
documentation, or not at all.  Supported options are:

  :documentation - The doc-string used for this class.

Options added to EIEIO:

  :allow-nil-initform - Non-nil to skip typechecking of null initforms.
  :custom-groups      - List of custom group names.  Organizes slots into
                        reasonable groups for customizations.
  :abstract           - Non-nil to prevent instances of this class.
                        If a string, use as an error string if someone does
                        try to make an instance.
  :method-invocation-order
                      - Control the method invocation order if there is
                        multiple inheritance.  Valid values are:
                         :breadth-first - The default.
                         :depth-first

Options in CLOS not supported in EIEIO:

  :metaclass - Class to use in place of `standard-class'
  :default-initargs - Initargs to use when initializing new objects of
                      this class.

Due to the way class options are set up, you can add any tags you wish,
and reference them using the function `class-option'."
  (declare (doc-string 4) (indent 2))
  `(progn
     ;; create a usual EIEIO class and tag it as jeison-odict class
     (defclass ,name ,superclasses ,slots :jeison-odict t ,@options-and-doc)
     ;; inject :path property into the newly created class
     (jeison-odict--set-paths ',name ',slots)))

(defun jeison-odictify (class-name)
  "Make a usual EIEIO CLASS-NAME jeison-odict compatible.

CLASS-NAME is a symbol name of the class, it must be EIEIO class.

We use this function to store additional information about slots
inside of them (inside of slots props), if this behavior doesn't fit
your needs, please consider declaring a separate class."
  ;; passing jeison-odict class would break custom `:path'es set in it
  (or (not (jeison-odict-class-p class-name))
      (error "Given type is already a jeison-odict class"))
  ;; check that the given type is EIEIO class indeed
  (cl-check-type (eieio--class-object class-name) eieio--class)
  ;; tag it as jeison-odict class
  (plist-put (eieio--class-options
              (jeison-odict--find-class class-name))
             :jeison-odict t)
  ;; and put :path property into its slots
  (mapc (lambda (slot-descriptor) (jeison-odict--set-path slot-descriptor nil))
        (jeison-odict--class-slots class-name)))

(defun jeison-odict-read (type alist-or-json &optional path)
  "Read TYPE from ALIST-OR-JSON by the given PATH.

TYPE is a CL-defined type (should work with `cl-typep'), that means
also that t is a valid type.
ALIST-OR-JSON is either a `string' with raw JSON or an `alist' representing
a JSON object where we want information to be parsed from.
PATH is a `list' of keys we should consequently find in JSON and
proceed with a nested JSON further on."
  ;; read JSON from string into an alist and proceed
  (let* ((json (if (stringp alist-or-json)
                   (json-parse-string alist-or-json :object-type 'alist :array-type 'list)
                 alist-or-json)))
    (jeison-odict--read-internal type json path)))

(define-error 'jeison-odict-wrong-parsed-type
  "Jeison-Odict encountered unexpected type" 'error)

(defun jeison-odict--read-internal (type json &optional path)
  "Read TYPE from JSON by the given PATH.

TYPE is a CL-defined type (should work with `cl-typep'), that means
also that t is a valid type.
JSON is an `alist' representing a JSON object where we want information
to be parsed from.
PATH is a `list' of keys we should consequently find in JSON and
proceed with a nested JSON further on."
  (let* ((json (jeison-odict--read-path json path))
         (result
          (pcase type
            ;; type is a jeison-odict class: read JSON as a class
            ((pred jeison-odict-class-p) (jeison-odict--read-class type json))
            ;; type is a homogeneous list of some sort
            (`(list-of ,element-type)
             ;; first we should check that JSON object itself is
             ;; some sort of sequence
             (or (cl-typep json 'sequence)
                 (signal 'jeison-odict-wrong-parsed-type
                         (list 'sequence json)))
             ;; then we should iterate of this JSON sequence
             ;; and parse element-type (retrieved from the type)
             ;; from each element
             (cl-mapcar (lambda (element)
                          (jeison-odict--read-internal element-type element)) json))
            (`(tagged-by ,tag ,options)
             (let* ((choice (alist-get tag json))
                    (chosen (alist-get (intern-soft choice) options)))
               (jeison-odict--read-internal chosen json)))
            (`(alist-of ,element-type)
             (or (cl-typep json 'cons)
                 (cl-typep json 'null)
                 (signal 'jeison-odict-wrong-parsed-type
                         (list 'cons json)))
             (cl-mapcar (pcase-lambda (`(,k . ,v))
                          `(,k . ,(jeison-odict--read-internal element-type v))) json))
            ;; not a special case of parsing - return whatever we found
            (_ json))))
    result))

(defclass jeison-odict--slot nil
  ((name :initarg :name
         :documentation "Name of the slot.")
   (initarg :initarg :initarg
            :initform nil
            :documentation "Constructor's argument of the slot.")
   (hasinitform :initarg :hasinitform
                :documentation "Does this slot have an initform")
   (path :initarg :path
         :documentation "JSON path to retrieve the slot.")
   (type :initarg :type
         :documentation "Lisp type of the slot."))
  :documentation
  "A small utility class containing information about jeison-odict class slots.")

(defun jeison-odict--read-class (class-name json)
  "Read jeison-odict CLASS-NAME from the given JSON.

CLASS-NAME is a symbol name of the class.
JSON is an `alist' representing a JSON object where we want information
to be parsed from."
  ;; iterate over the class' slots and read data from JSON to fill them
  (-let* ((slots (jeison-odict--get-slots class-name))
          ((initarg-slots rest-slots) (--separate (oref it initarg) slots))
          (constructor-arguments
           (-mapcat (lambda (slot) (jeison-odict--read-slot slot json)) initarg-slots))
          (setter-arguments
           (mapcar (lambda (slot) (jeison-odict--read-slot slot json)) rest-slots))
          ;; construct the class with parsed arguments
          (result (apply class-name constructor-arguments)))
    ;; set values to slots without `:initarg's
    (jeison-odict--set-slot-values result setter-arguments)
    result))

(defun jeison-odict--set-slot-values (object list-of-arguments)
  "Set slots of the OBJECT using the given LIST-OF-ARGUMENTS.

LIST-OF-ARGUMENTS is a list of lists ((name-1 value-1) .. (name-n value-n)),
where name-i is a name of OBJECT's slot and value-i is its new value."
  (mapc (lambda (arguments)
          (-let (((name value) arguments))
            (eieio-oset object name value)))
        list-of-arguments))

(defun jeison-odict--read-slot (slot json)
  "Return a cons of arguments to initialize the given SLOT in a constructor.

It returns a list of a form (:field field-value) to be later used as arguments
in the target class' constructor:
  (class :field1 field1-value :field2 field2-value ...).

SLOT is a jeison-odict descriptor of an slot, i.e. `jeison-odict--slot'.
JSON is an `alist' representing a JSON object where we want information
to be parsed from."
  (let ((slot-value (jeison-odict--read-internal
                     (oref slot type) json (oref slot path))))
    (if (and (null slot-value) (oref slot hasinitform))
        nil
      (list (or (oref slot initarg)
                (oref slot name))
            slot-value))))

(defun jeison-odict--read-path (json path)
  "Return nested JSON object found by the given list of keys PATH.

JSON is an `alist' representing JSON object.
PATH is a `list' of keys we should consequently find in JSON and
proceed with a nested JSON further on."
  (pcase path
    ;; unwrap one level of the path
    (`(,head . ,tail) (jeison-odict--read-path (jeison-odict--read-element head json) tail))
    ;; the path is empty - nothing left to do here
    ('nil json)
    ;; path is not a list - assume that it's a one-level path
    (_ (jeison-odict--read-element path json))))

(defun jeison-odict--read-element (element json)
  "Read ELEMENT from the given JSON.

ELEMENT is either a name of the field (`symbol' or `string'), or an index
of the required element of the sequence.
JSON is an `alist' representing JSON object."
  (cl-typecase element
    ;; symbol -> it's a simple key
    (symbol (assoc-default element json))
    ;; string -> it's the same semantics as with the symbol case,
    ;; but keys in alist are symbols and trying to get values by plain
    ;; string won't do, so we convert string to symbol
    (string (jeison-odict--read-element (intern element) json))
    ;; integer -> it looks like json should be a sequence
    ;; and we want just a single element of it
    ;; additionally, if it's negative treat it as an index from the end
    (integer (elt json (if (< element 0)
                           (+ (length json) element) element)))
    ;; list -> it is a function call with path elements as its arguments
    (list (jeison-odict--apply-function element json))))

(defun jeison-odict--apply-function (function-and-args json)
  "Parse FUNCTION-AND-ARGS and apply it to JSON.

FUNCTION-AND-ARGS is a list of the following structure:
  (FUNCTION . (ARGUMENT-PATHS))
FUNCTION can be a function symbol or a lambda expression.
ARGUMENT-PATHS is a list of paths (that can also include functions)
telling jeison-odict where it can find arguments for the FUNCTION."
  (-let* (((function-element . argument-paths) function-and-args)
          ;; find arguments for the function by the specified paths...
          (arguments (mapcar (lambda (path) (jeison-odict--read-path json path))
                             argument-paths)))
    ;; ...and return whatever is the result of this function
    (apply function-element arguments)))

(defun jeison-odict--set-paths (class-or-class-name slots)
  "Save path information from SLOTS into CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself.
SLOTS is a list of slot descriptions given by the user to
`jeison-odict-defclass' macro.

We use this function to store additional information about slots
inside of them."
  (cl-mapcar #'jeison-odict--set-path
             (jeison-odict--class-slots class-or-class-name) slots))

(defun jeison-odict--set-path (cl-slot slot-description)
  "Save path information into the given SL-SLOT.

CL-SLOT is an object representing `cl--slot-descriptor' class.
SLOT-DESCRIPTION is a description of the slot given by the user to
`jeison-odict-defclass' macro.

Jeison-Odict stores path information in the slot's props because props are
just a simple `alist' and we can sneak additional information in there."
  (let* ((slot-options (cdr slot-description))
         ;; path is either user-defined :path
         (path (or (plist-get slot-options :path)
                   ;; or just a name of the slot
                   (cl--slot-descriptor-name cl-slot)))
         ;; retrieve props from the slot
         (slot-props (cl--slot-descriptor-props cl-slot))
         ;; add extracted :path to the props
         (slot-props (append slot-props `((:path . ,path)))))
    ;; save new version of props back into the slot
    (setf (cl--slot-descriptor-props cl-slot) slot-props)))

(defun jeison-odict-class-p (class-or-class-name)
  "Return t if CLASS-OR-CLASS-NAME is a jeison-odict class.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (let ((class (jeison-odict--find-class class-or-class-name)))
    (and (eieio--class-p class)
         (plist-get (eieio--class-options class) :jeison-odict))))

(defun jeison-odict-object-p (object)
  "Return t if OBJECT represents jeison-odict class."
  ;; check that it's EIEIO object first
  (and (cl-typep object 'eieio-object)
       ;; and only then check the class itself
       (jeison-odict-class-p (eieio-object-class object))))

(defun jeison-odict--get-slots (class-or-class-name)
  "Construct a list of `jeison-odict--slot' for all slots from CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (let* ((class (jeison-odict--find-class class-or-class-name))
         ;; compose a list of `jeison-odict--slot' objects
         (class-slots (mapcar #'jeison-odict--get-slot (jeison-odict--class-slots class)))
         ;; retrieve a list of initargs from EIEIO
         (initargs (jeison-odict--initargs class)))
    ;; match slots and initargs
    (jeison-odict--set-initargs class-slots initargs)
    ;; return the list of slots
    class-slots))

(defun jeison-odict--get-slot (raw-slot)
  "Construct `jeison-odict--slot' object from RAW-SLOT and INITARG.

RAW-SLOT is a `cl--slot-descriptor' object of a jeison-odict class.
INITARG is `:initarg' of RAW-SLOT, EIEIO keeps them separately."
  (let ((props (cl--slot-descriptor-props raw-slot)))
    (jeison-odict--slot :name (cl--slot-descriptor-name raw-slot)
                  :hasinitform (eq (cl--slot-descriptor-initform raw-slot) 'eieio--unbound)
                  :type (cl--slot-descriptor-type raw-slot)
                  :path (assoc-default :path props))))

(defun jeison-odict--initargs (class-or-class-name)
  "Return list of `:initarg's associated with the given CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (eieio--class-initarg-tuples (jeison-odict--find-class class-or-class-name)))

(defun jeison-odict--set-initargs (class-slots initargs)
  "Set initarg field for the given CLASS-SLOTS using INITARGS.

CLASS-SLOTS is a list of `jeison-odict--slot' objects.
INITARGS is a list of cons (initarg . field-name) from EIEIO.

As INITARGS has cons only for the fields that do have initargs in the first
place, common zip of two lists won't work.  However, all the elements from
INITARGS are guaranteed to be in CLASS-SLOTS.  INITARGS and CLASS-SLOTS are
also guaranteed to have the same order."
  ;; if at least of these lists is empty, we have not much to do here
  (when (and class-slots initargs)
    (-let (((slot . rest-slots) class-slots)
           (((initarg . name) . rest-initargs) initargs))
      ;; check that current initarg is defined for the current slot
      (when (equal (oref slot name) name)
        ;; set initarg field for the slot
        (oset slot initarg initarg)
        ;; pop the top initarg from the list
        ;; as we matched it already
        (setq initargs rest-initargs))
      ;; match the rest of slots
      (jeison-odict--set-initargs rest-slots initargs))))

(defun jeison-odict--class-slots (class-or-class-name)
  "Return slot objects associated with the given CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (eieio--class-slots (jeison-odict--find-class class-or-class-name)))

(defun jeison-odict--find-class (class-or-class-name)
  "Return class object for the given CLASS-OR-CLASS-NAME.

CLASS-OR-CLASS-NAME can be a symbol name of the class or class itself."
  (if (symbolp class-or-class-name)
      (cl-find-class class-or-class-name)
    class-or-class-name))

(provide 'jeison-odict)
;;; jeison-odict.el ends here
