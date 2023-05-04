(defmacro aif (test-form then-form &optional else-form)
`(let ((it ,test-form))
(if it ,then-form ,else-form)))


(defstruct binding-form name match-fn body-forcer-fn)
(defparameter *installed-binding-forms* nil)

(defun install-binding-form (name match-fn body-forcer-fn)
  (push (make-binding-form :name name :match-fn match-fn
                           :body-forcer-fn body-forcer-fn )
         *installed-binding-forms* ) )

(defun binding-exp-p (exp)
  (if (not (listp exp))
      nil
      (some #'(lambda (binding-form)
                (if (funcall (binding-form-match-fn binding-form) exp)
                    binding-form ) )
            *installed-binding-forms* ) ) )

(defun delay-forcer (force-vars exp)
  (aif (binding-exp-p exp)
       (funcall (binding-form-body-forcer-fn it) force-vars exp)
       (cond ((null exp) nil)
             ((force-exp-p exp) exp) ;; no need to re-force a variable
             ((listp exp)
              (cons (delay-forcer force-vars (car exp))
                    (delay-forcer force-vars (cdr exp)) ) )
             ((symbolp exp)
              (if (member exp force-vars :test #'eq)
                  (list 'force exp)
                   exp ) )
             (t exp) ) ) )

(defmacro with-forced-vars (var-list &body body)
  `(progn ,@(mapcar #'(lambda (exp) (delay-forcer var-list exp) )
                    body )) )
                    
(defun forced-param-p (param)
  (and (symbolp param)
       (> (length (symbol-name param)) 2)
       (string-equal (symbol-name param)
                     "f:"
                     :start1 0
                     :end1 2 ) ) ) 

(defun get-forced-param-name (fp)
  (read-from-string (subseq (symbol-name fp) 2)) )


(defmacro flambda (params &body body)
  (let ((forced-vars (loop for param in params
                          when (forced-param-p param)
                          collect (get-forced-param-name param) ))
        (lambda-list (loop for param in params
                          collect (if (forced-param-p param)
                                      (get-forced-param-name param)
                                      param ) )) )
    `#'(lambda ,lambda-list
         (with-forced-vars ,forced-vars ,@body) ) ) )


(defmacro fdefun (name params &body body)
  (let ((forced-vars (loop for param in params
                          when (forced-param-p param)
                          collect (get-forced-param-name param) ))
        (lambda-list (loop for param in params
                          collect (if (forced-param-p param)
                                      (get-forced-param-name param)
                                      param ) )) )
    `(defun ,name ,lambda-list
       (with-forced-vars ,forced-vars ,@body) ) ) )

; lambda-form installation code:
(defun lambda-matcher-fn (exp)
  (and (listp exp)
       (eq (car exp) 'lambda) ) )

(defun lambda-forcer-fn (forced-vars exp)
  (let* ((lambda-vars (cadr exp))
         (new-forced-vars (set-difference
                             forced-vars
                             lambda-vars ))
         (new-body-exps (mapcar #'(lambda (e)
                                    (delay-forcer new-forced-vars e) )
                                (cddr exp)) ) )
  `(lambda ,lambda-vars ,@new-body-exps) ) )

(install-binding-form
  'lambda
  #'lambda-matcher-fn
  #'lambda-forcer-fn )

; let-form installation code:
(defun let-matcher-fn (exp)
  (and (listp exp)
       (eq (car exp) 'let) ) )

(defun let-forcer-fn (forced-vars exp)
  (let* ((old-bindings (cadr exp))
         (let-vars (mapcar #'(lambda (b)
                               (if (symbolp b)
                                   b
                                   (car b) ) )
                           old-bindings ))
         (let-vals (mapcar #'(lambda (b)
                               (if (symbolp b)
                                   nil
                                   (delay-forcer forced-vars (cadr b)) ) )
                           old-bindings ))
         (new-forced-vars (set-difference forced-vars let-vars))
         (new-bindings (mapcar #'(lambda (var val) (list var val)) let-vars let-vals))
         (new-body-exps (mapcar #'(lambda (e)
                                    (delay-forcer new-forced-vars e) )
                               (cddr exp) )) )
    `(let ,new-bindings
       ,@new-body-exps ) ) )

(install-binding-form
  'let
  #'let-matcher-fn
  #'let-forcer-fn )

; With-forced-vars -form installation:
(defun with-forced-vars-matcher-fn (exp)
  (and (listp exp)
       (eq (car exp) 'with-forced-vars) ) )

(defun with-forced-vars-forcer-fn (forced-vars exp)
  (let* ((dont-force-these (cadr exp))
         (force-these (set-difference forced-vars
                                      dont-force-these) )
         (new-body-exps (mapcar #'(lambda (e)
                                    (delay-forcer force-these e) )
                                (cddr exp)) ) )
  `(with-forced-vars ,@new-body-exps) ) )

(install-binding-form
  'with-forced-vars
  #'with-forced-vars-matcher-fn
  #'with-forced-vars-forcer-fn )

;; The new flambda installation code:
(defun flambda-matcher-fn (exp)
  (and (listp exp) (eq (car exp) 'flambda)) )

(defun flambda-forcer-fn (forced-vars exp) 
  (let* ((dont-force-these (mapcar #'(lambda (p)
                                       (if (forced-param-p p)
                                           (get-forced-param-name p)
                                           P ) )
                                   (cadr exp) ) )
         (force-these (set-difference forced-vars dont-force-these))
         (new-body-exps (mapcar #'(lambda (e) (delay-forcer force-these e))
                                (cddr exp) ) ) )
    `(flambda ,(cadr exp)
       ,@ new-body-exps ) ) )
       
(install-binding-form
  'flambda
  #'flambda-matcher-fn
  #'flambda-forcer-fn )


;; The new fdefun installation code:
(defun fdefun-matcher-fn (exp)
  (and (listp exp) (eq (car exp) 'fdefun)) )

(defun fdefun-forcer-fn (forced-vars exp) 
  (let* ((dont-force-these (mapcar #'(lambda (p)
                                       (if (forced-param-p p)
                                           (get-forced-param-name p)
                                           P ) )
                                   (caddr exp) ) )
         (force-these (set-difference forced-vars dont-force-these))
         (new-body-exps (mapcar #'(lambda (e) (delay-forcer force-these e))
                                (cdddr exp) ) ) )
    `(fdefun ,(cadr exp) ,(caddr exp)
       ,@ new-body-exps ) ) )

(install-binding-form
  'fdefun
  #'fdefun-matcher-fn
  #'fdefun-forcer-fn )
