;;;; utils.lisp

(in-package #:utils)

(defun read-lines (filename &key (parser #'identity))
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line collect (funcall parser line))))

(defun sum (&optional (list (list 0)))
  (reduce #'+ list))

(defun head (sequence &optional (end 1))
  (if (= end 1) (elt sequence 0) (subseq sequence 0 end)))

(defun tail (sequence &optional (start 1))
  (subseq sequence start))

(defun mappend (fn &rest lists)
  "like mapcan, but non-destructive"
  (apply #'append (apply #'mapcar fn lists)))

(defun extract-if (pred tree)
  (labels ((rec (tree acc)
             (cond ((null tree) acc)
                   ((consp (car tree))
                    (rec (cdr tree) (append (rec (car tree) nil) acc)))
                   (t (if (funcall pred (car tree))
                          (rec (cdr tree) (cons (car tree) acc))
                          (rec (cdr tree) acc))))))
    (nreverse (rec tree nil))))

(defun mapconcat (result-type function list &rest lists)
  "map FUNCTION over one or more lists and concatenate all the results into a RESULT-TYPE"
  (apply #'concatenate result-type
         (apply #'map 'list function list lists)))

(defun split-string (string &key max (sep '(#\Space #\Tab)))
  (block ()
    (let ((list nil) (words 0) (end (length string)))
      (when (zerop end) (return nil))
      (flet ((separatorp (char) (find char sep))
             (done () (return (cons (subseq string 0 end) list))))
        (loop for start = (if (and max (>= words (1- max)))
                              (done)
                              (position-if #'separatorp string :end end :from-end t))
              do (when (null start) (done))
                 (let ((sub (subseq string (1+ start) end)))
                   (unless (equal "" sub)
                     (push (subseq string (1+ start) end) list)))
                 (incf words)
                 (setf end start))))))

(defun split-list (delim list &key (test #'equal))
  (loop with group = nil and result = nil 
        for x in (reverse list) 
        if (funcall test x delim) do (push group result) (setf group nil)
        else do (push x group)
        finally (return (if group (push group result) result))))

(defmacro with-gensyms (syms &body body)
  "creates a gensym for each symbol in syms"
  `(let ,(mapcar (lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro with-anaphora (syms &body body)
  "interns symbols for using with anaphoric macros. similar to with-gensyms"
  `(let ,(mapcar (lambda (s) `(,s (intern (symbol-name ',s)))) syms)
     ,@body))

(defun rbuild (expr)
  "helper function for fn macro"
  (cond ((or (atom expr) (eq (car expr) 'lambda)) expr)
        ((eq (car expr) ':c) (build-compose (cdr expr)))
        (t (build-call (car expr) (cdr expr)))))

(defun build-call (op fns &aux (g (gensym)))
  "helper function for fn macro"
  `(lambda (,g) (,op ,@(loop for f in fns collect `(,(rbuild f) ,g)))))

(defun build-compose (fns &aux (g (gensym)))
  "helper function for fn macro"
  `(lambda (,g)
     ,(labels ((rec (fns)
                 (if (null fns) g
                     `(,(rbuild (car fns)) ,(rec (cdr fns))))))
        (rec fns))))

(defmacro fn (expr)
  "general function building macro.
   usage: (fn (op argfn1 argfn2 etc)) => (lambda (x) (op (argfn1 x) (argfn2 x) (etc x)))
   you can also nest calls or use the :c keyword to compose them:
   (fn (:c fun1 fun2)) => (lambda (x) (fun1 (fun2 x)))"
  `,(rbuild expr))

(defun condlet-binds (vars cl)
  "helper function for condlet macro. not exported"
  (mapcar (lambda (bindform)
            (if (consp bindform)
                (cons (cdr (assoc (car bindform) vars))
                      (cdr bindform))
                (cdr (assoc bindform vars))))
          (cdr cl)))

(defun condlet-clause (vars cl bodfn)
  "helper function for condlet macro. not exported"
  (let* ((binds   (cdr cl))
         (bound   (mapcar (lambda (x) (if (consp x) (car x) (identity x))) binds))
         (missing (remove-if (lambda (x) (member x bound)) (mapcar #'car vars))))
    `(,(car cl) (let* ,(condlet-binds vars (append cl missing))
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defmacro condlet (clauses &body body &aux (bodfn (gensym)))
  "like let, but choose which variable bindings to use based on cond-like forms
variables not in the chosen bindings will be set to NIL.
usage: (condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
                 ((= 1 1) (y (princ 'c)) (x (princ 'd)))
                 (t       (x (princ 'e)) (z (princ 'f)))))
       ;=> (D C Nil)"
  (let* ((binds (mappend #'cdr clauses))
         (all   (mapcar (lambda (x) (if (consp x) (car x) (identity x))) binds))
         (vars  (mapcar (lambda (v) (cons v (gensym))) (remove-duplicates all))))
    `(labels ((,bodfn ,(mapcar #'car vars) ,@body))
       (cond ,@(mapcar (lambda (cl) (condlet-clause vars cl bodfn)) clauses)))))

(defmacro any (&rest forms &aux (result (gensym)))
  "like OR but without short circuit"
  `(let (,result)
     ,@(mapcar (lambda (f) `(if ,f (setf ,result t))) forms)
     ,result))

(defmacro all (&body forms &aux (result (gensym)))
  "like AND but without short circuit"
  `(let ((,result t))
     ,@(mapcar (lambda (f) `(unless ,f (setf ,result nil))) forms)
     ,result))

(defmacro aif (test-form then-form &optional else-form)
  "anaphoric if statement"
  (with-anaphora (it)
    `(let ((,it ,test-form))
       (if ,it ,then-form ,else-form))))

(defmacro alambda (parms &body body)
  "anaphoric lambda for making recursive lambdas"
  (with-anaphora (self)
   `(labels ((,self ,parms ,@body)) #',self)))

(defun memoize (fn)
  "create a function with a cache for saving results"
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win val (setf (gethash args cache) (apply fn args)))))))

(defun iota (count &key (start 0) (step-fun #'1+))
  "make list of length COUNT with numbers from START stepping with STEP-FUN"
  (loop repeat count for x = start then (funcall step-fun x) collect x))

(defun range (x y)
  (iota (1+ (- y x)) :start x))

(defun array-map (function array &optional (result (make-array (array-dimensions array))))
  "map FUNCTION across ARRAY of any dimensions"
  (dotimes (i (array-total-size array) result)
    (setf (row-major-aref result i)
          (funcall function (row-major-aref array i)))))

(defun group (seq psize &key (until (lambda (&rest args) (declare (ignorable args)) nil)))
  "split a sequence into groups of length PSIZE while (UNTIL group) evals to nil"
  (loop with len = (length seq)
        for start from 0 by psize below len
        for group = (subseq seq start (min len (+ start psize)))
        collect group until (funcall until group)))


(defun flatten (x)
  "remove all nested parens in a list"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro nif (expr pos zero neg &aux (g (gensym)))
  "numeric if. returns either POS, ZERO, or NEG depending on the value of EXPR"
  `(let ((,g ,expr))
     (cond ((plusp ,g) ,pos)
           ((zerop ,g) ,zero)
           (t ,neg))))

(defmacro allf (val &rest args &aux (gval (gensym)))
  "setf all places in args to val"
  `(let ((,gval ,val))
     (setf ,@(mapcan (lambda (a) (list a gval)) args))))

(defmacro nilf (&rest args)
  "setf all places in args to nil"
  `(allf nil ,@args))

(defmacro tf (&rest args)
  "setf all places in args to t"
  `(allf t ,@args))

(define-modify-macro toggle2 () not)

(defmacro toggle (&rest args)
  "toggle all places in args. nil -> t, t -> nil"
  `(progn ,@(mapcar (lambda (a) `(toggle2 ,a)) args)))

(defmacro _f (op place &rest args)
  "destructively apply op to place"
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro -> (&rest forms)
  "-> threading macro from clojure"
  (let ((outer (if (listp (cadr forms)) (cadr forms)
                   `(,(cadr forms)))))
    (if (null (cadr forms)) (car forms)
        `(-> (,(car outer) ,(car forms) ,@(cdr outer))
             ,@(cddr forms)))))

(defmacro ->> (&rest forms)
  "->> threading macro from clojure"
  (let ((outer (if (listp (cadr forms)) (cadr forms)
                   `(,(cadr forms)))))
    (if (null (cadr forms)) (car forms)
        `(->> (,@outer ,(car forms))
              ,@(cddr forms)))))

(defmacro as-> (init var &rest forms)
  "as-> threading macro from clojure"
  (let ((outer (if (listp (car forms)) (car forms)
                   `(,(car forms) ,var))))
    (if (null forms) init
        `(as-> ,(subst init var outer) ,var
               ,@(cdr forms)))))

(defmacro cond-> (init &rest forms)
  "cond-> threading macro from clojure"
  (let ((outer (if (listp (cadr forms)) (cadr forms)
                   `(,(cadr forms)))))
    (if (null forms) init
        `(cond-> (if ,(car forms) (-> ,init ,outer) ,init)
                 ,@(cddr forms)))))

(defmacro cond->> (init &rest forms)
  "cond->> threading macro from clojure"
  (let ((outer (if (listp (cadr forms)) (cadr forms)
                   `(,(cadr forms)))))
    (if (null forms) init
        `(cond->> (if ,(car forms) (->> ,init ,outer) ,init)
                  ,@(cddr forms)))))

(defmacro pull (obj place &rest args &aux (g (gensym)))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* ((,g ,obj)
            ,@(mapcar #'list vars forms)
            (,(car var) (delete ,g ,access ,@args)))
       ,set)))

(defmacro pull-if (test place &rest args &aux (g (gensym)))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* ((,g ,test)
            ,@(mapcar #'list vars forms)
            (,(car var) (delete-if ,g ,access ,@args)))
       ,set)))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))

(defun trec (rec &optional (base #'identity))
  (alambda (tree)
    (cond ((atom tree) (if (functionp base) (funcall base tree) base))
          (t (funcall rec tree
                      (lambda () (self (car tree)))
                      (lambda () (if (cdr tree) (self (cdr tree)))))))))

(defmacro atrec (rec &optional (base 'it))
  (with-gensyms (lfn rfn)
    (with-anaphora (left right it)
      `(trec (lambda (,it ,lfn ,rfn) (declare (ignorable ,it))
               (symbol-macrolet ((,left (funcall ,lfn))
                                 (,right (funcall ,rfn)))
                 ,rec))
             (lambda (,it) (declare (ignorable ,it)) ,base)))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

(define-modify-macro concf (obj) nconc)

(defmacro in (obj &rest choices &aux (insym (gensym)))
  `(let ((,insym ,obj))
     (or ,@(mapcar (lambda (c) `(eql ,insym ,c)) choices))))
