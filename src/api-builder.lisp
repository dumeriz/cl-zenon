(defpackage :api-builder
  (:use :cl)
  (:export :define-api-type
	   :define-api
	   :make-define-api-macro
	   :extract-from-hashmap
	   :zenon-api-result
	   :zenon-api-list-result
	   :zenon-api-paged-result))

(in-package :api-builder)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lispify-string (string)
    "Returns a lispified representation of `STRING`. Uppercase letters are replaced with
lowercase, with dash prepended, unless it is the initial character which is only lowercased. Underscores are replaced with dashes."
    (let ((param-string (str:param-case string)))
      (if (char= (elt string 0) #\_)
	  (str:concat "-" param-string)
	  param-string)))

  (defun make-keyword (name)
    (values (intern (string-upcase name) "KEYWORD")))

  (defun str->varname (str)
    (string-upcase (lispify-string str)))

  (defun api-value-ctor-name (cls)
    "Return the canonical name of the ctor for api class `CLS`."
    (str->varname (str:concat "MAKE-" (symbol-name cls))))

  (defun get-slot-names (spec)
    (loop for s in spec collecting (if (listp s) (car s) s))))

(defun slot-spec-cls-table (spec)
  "From the slot description `SPEC`, creates a hash table with keys being the
names from `SPEC` and value being a api type, a function, a two-element list,
or `IDENTITY`.
Each entry in `SPEC` may be a single string or a list with the name
in the first position and an existing api type or a function in the last.
See `SET-SLOT-VALUES` for usage."
  (let ((ht (make-hash-table :test #'equal)))
    (mapcar #'(lambda (s)
		(cond ((and (listp s) (fboundp (second s)))
		       (setf (gethash (first s) ht) (symbol-function (second s))))
		      ((listp s)
		       (setf (gethash (first s) ht) (second s)))
		      (t (setf (gethash s ht) #'identity))))
	    spec)
    ht))

(defun set-slot-values (instance slot-cls-ht data-ht &optional (package *package*))
  "Set the slots of `INSTANCE` from `DATA-HT` using the type hints from `SLOT-CLS-HT`.
`SLOT-CLS-HT` is a table mapping keys from `DATA-HT` to previously defined types from
the api (i.e., a function `MAKE-...` must exist for that type) or a function which
must accept a single value.
Note that the symbols (ctor and slot names) are interned in the current package
unless `PACKAGE` is given."
  (flet ((get-value (ctor-or-type data)
	   (let ((ctor (cond
			 ((functionp ctor-or-type)
			  ctor-or-type)
			 ((symbolp ctor-or-type)
			  (intern (api-value-ctor-name ctor-or-type) package)))))
	     (funcall ctor data))))
    (alexandria:maphash-keys
     #'(lambda (k)
	 (let* ((ctor-or-type (gethash k slot-cls-ht))
		(data (gethash k data-ht)))
	   (setf (slot-value instance (intern (str->varname k) package))
		 (cond
		   ((listp data)
		    (mapcar #'(lambda (d) (get-value ctor-or-type d)) data))
		   (t
		    (get-value ctor-or-type data))))))
     data-ht)))

(defclass zenon-api-result () ())

(defclass zenon-api-list-result (zenon-api-result)
  ((list :initarg :list :reader :list)
   (count :initarg :count :reader :count)))

(defclass zenon-api-paged-result (zenon-api-list-result)
  ((more :initarg :more :reader :more)))

(defmacro define-api-type (classname &rest slot-spec)
  "Generates a derivative of `ZENON-API-RESULT` for symbol `CLASSNAME` and a ctor.
`CLASSNAME` will be created with a slot for each entry in `SLOT-SPEC`, where
entry is either a simple string or a `CONS` with a string in the `CAR` place and a
previously defined api type in the `CDR`. The slot will have an initarg of the same
symbol name and a reader of the corresponding keyword name.

ctor is named as make-`CLASSNAME` with a single hashtable argument `DATA` 
and an optional keyword argument `AGGREGATE`, which may be `NIL`, `LIST`, `ARRAY`
or `PAGED`. That keyword argument must be chosen according to the type returned
from the Zenon API. ctor then parses `DATA` and generates either a single instance
of type `CLASSNAME` (if `AGGREGATE` is NIL), or a `ZENON-API-LIST-RESULT` (for :list)`,
`ZENON-API-PAGED-RESULT (for :paged), or a list of `CLASSNAME` (for :array)."
  (let ((slot-names (get-slot-names slot-spec))
	(package *package*))
    `(progn
       ;; defines a class <classname> with slots from slot-spec
       (defclass ,classname (zenon-api-result)
	 (,@(loop
	       for s in slot-names for symname = (str->varname s)
	       collecting `(,(intern symname)
			     :reader ,(intern symname "KEYWORD")
			     :initarg ,(make-keyword symname)))))
       ;; defines a make-<classname> method expecting a hashtable
       (defun ,(intern (api-value-ctor-name classname))
	   (data &key (aggregate nil))
	 (labels ((single (ht)
		    (let ((object (make-instance ',classname)))
		      (set-slot-values object
				       (slot-spec-cls-table ',slot-spec)
				       ht
				       ,package)
		      object))
		  (multiple (lst)
		    (mapcar #'(lambda (entry) (single entry)) lst)))
	   (case aggregate
	     (:list (make-instance 'zenon-api-list-result
				   :list (multiple (gethash "list" data))
				   :count (gethash "count" data)))
	     (:array (multiple data))
	     (:paged (make-instance 'zenon-api-paged-result
				    :list (multiple (gethash "list" data))
				    :count (gethash "count" data)
				    :more (gethash "more" data)))
	     (otherwise (single data))))))))

(defmacro define-api (name
		      method
		      &key (args nil) (type nil) (aggregate nil))
  "Defines an api method `NAME` that calls `METHOD` with `ARGS`, returning `TYPE`.
If `TYPE` is not given, the unmodified result is returned (a hashmap). If it is given,
it must be a type that previously has been defined with `DEFINE-API-TYPE`.
If `AGGREGATE` is not `NIL`, it must be `:LIST` or `:PAGED` or `:ARRAY`. One of these must be
defined for api calls that return a list, paged or array result. The latter results in
a list of `TYPE` instances. `:LIST` calls return a `ZENON-API-LIST-RESULT`, with the `LIST`
slot set to a list of `TYPE` instances. `:PAGED` calls return a `ZENON-API-PAGED-RESULT`."
  (let ((fn (if type
		(intern (api-value-ctor-name type))
		'(lambda (&rest ignored)))))
    (alexandria:with-gensyms ((client "client"))
      `(progn
	 (defun ,(intern (symbol-name name))
	     ,(if args `(,client ,@args) `(,client))
	   (if ',type
	       (funcall #',fn
			(jsonrpc:call ,client ,method (list ,@args))
			:aggregate ,aggregate)
	       (jsonrpc:call ,client ,method (list ,@args))))
	 (export ',(intern (symbol-name name)))))))

(defmacro make-define-api-macro (prefix)
  (let ((name (string-upcase (str:concat "define-" (str:replace-all "." "-" prefix) "-api"))))
    `(defmacro ,(intern name) (name &key (args nil) (type nil) (aggregate nil))
       (let ((method (str:concat ,prefix "." name))
	     (fn (intern (string-upcase (str:param-case name)))))
	 `(define-api ,fn ,method :args ,args :type ,type :aggregate ,aggregate)))))
       
(defun extract-from-hashmap (data type)
  (let ((fn (intern (api-value-ctor-name type))))
    (loop
       with ht = (make-hash-table)
       for k being the hash-keys of data using (hash-value v)
       do (setf (gethash k ht) (funcall fn v))
       finally (return ht))))

