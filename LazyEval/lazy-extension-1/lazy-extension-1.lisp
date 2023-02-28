(defmacro aif (test-form then-form &optional else-form)
`(let ((it ,test-form))
(if it ,then-form ,else-form)))


(defstruct binding-form name match-fn body-forcer-fn)
;; Each installed binding form (lambda, let, etc...) must have
;; 1. a name
;; 2. a binding form matcher function
;; 3. a binding form "body forcer function" that will transform
;;    all the free variables that are being automatically
;;    forced by the surrounding code into forced variables.

(defparameter *installed-binding-forms* nil)

(defun install-binding-form (name match-fn body-forcer-fn)
  "Installs a new binding form recognizable by the system"
  (push (make-binding-form :name name :match-fn match-fn
                           :body-forcer-fn body-forcer-fn )
         *installed-binding-forms* ) )

(defstruct delay forced closure)

(defun binding-exp-p (exp)
  "Returns true if exp is a binding form recognized by the language,
   false otherwise."
  (if (not (listp exp))
      nil
      (some #'(lambda (binding-form)
                (if (funcall (binding-form-match-fn binding-form) exp)
                    binding-form ) )
            *installed-binding-forms* ) ) )


(defun delay-forcer (force-vars exp)
  "The 'heart' of the system: ensures the automatic forcing of the
   correct variables"
  (aif (binding-exp-p exp)
       (funcall (binding-form-body-forcer-fn it) force-vars exp)
       (cond ((null exp) nil)
             ((force-exp-p exp) exp) ;; if exp is already forced, don't try re-forcing it.
             ((listp exp)
              (cons (delay-forcer force-vars (car exp))     ;; for lists, traverse all the leaves on both the car
                    (delay-forcer force-vars (cdr exp)) ) ) ;; and the cdr positions
             ((symbolp exp)
              (if (member exp force-vars) ;; for a SYMBOL that equals one of the forcable symbols,
                  (list 'force exp)       ;; force the symbol
                   exp ) )     ;; If exp doesn't equal any of the forcable variable names
             (t exp) ) ) )     ;; or if exp is not anything that can be forced, do nothing to exp


;; The primitives of the language:
;; (see README.md for more details of the syntaxes and use examples)
(defmacro with-forced-vars (var-list &body body)
  (if (null (cdr body))
      (delay-forcer var-list (car body))
      `(progn ,@(mapcar #'(lambda (exp)
                            (delay-forcer var-list exp) )
                        body )) ) )
;; TODO: with-forced-vars is not actually recognized by the system yet,
;;       so install it into the system. (See README.md) for why this isn't
;;       technically a problem but should still be fixed...
  
(defmacro flambda (forced-var-list params &body body)
  (let ((params (if (eq params :repeat) forced-var-list params)))  ;; careful with shared list structures, Eugene!
    `#'(lambda ,params
       (with-forced-vars ,forced-var-list ,@body) ) ) )

(defmacro fdefun (name forced-var-list params &body body)
  (let ((params (if (eq params :repeat) forced-var-list params)))  ;; careful with shared list structures, Eugene!
    `(defun ,name ,params
       (with-forced-vars ,forced-var-list ,@body) ) ) )

(defmacro fflambda (params &body body)
  `(flambda ,params :repeat ,@body) )
    
(defmacro ffdefun (name params &body body)
  `(fdefun ,name ,params :repeat ,@body) )

  
;; Next, install the lambda-expression into the system:
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
  


;; Next, install the let-expression into the system:
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
  

;; Next, install the flambda-expression into the system:
;;
;; for reference, the parameter positions in flambda call:
;;
;; (flambda forced-var-list params body-1 body-2 ... body-n)
;; (car     cadr            caddr  cdddr...................)
(defun flambda-matcher-fn (exp)
  (and (listp exp)
       (eq (car exp) 'flambda) ) )
       
(defun flambda-forcer-fn (forced-vars exp)  
  (if (eq (caddr exp) :repeat)
      `(flambda ,(union forced-vars (cadr exp)) ,(cadr exp) ,@(cdddr exp))
      `(flambda ,(union (set-difference forced-vars (caddr exp)) (cadr exp))
                ,(caddr exp)
                ,@(cdddr exp) ) ) )

(install-binding-form
  'flambda
  #'flambda-matcher-fn
  #'flambda-forcer-fn )
  
  
;; Next, install the fflambda-expression into the system:
;;
;; for reference, the parameter positions in fflambda call:
;;
;; (flambda forced-params body-1 body-2 ... body-n)
;; (car     cadr          cddr...................)
(defun fflambda-matcher-fn (exp)
  (and (listp exp)
       (eq (car exp) 'fflambda) ) )

(defun fflambda-forcer-fn (forced-vars exp)  
  `(flambda ,(union forced-vars (cadr exp)) ,(cadr exp) ,@(cdddr exp)) )
  
(install-binding-form
  'fflambda
  #'fflambda-matcher-fn
  #'fflambda-forcer-fn )



;; Next, install the fdefun-expression into the system:
;;
;; for reference, the parameter positions in fdefun call:
;;
;; (fdefun name forced-var-list params body-1 body-2 ... body-n)
;; (car    cadr caddr           cadddr cddddr..................)
(defun fdefun-matcher-fn (exp)
  (and (listp exp)
       (eq (car exp) 'fdefun) ) )

(defun fdefun-forcer-fn (forced-vars exp)  
  (if (eq (cadddr exp) :repeat)
      `(fdefun ,(cadr exp) ,(union forced-vars (caddr exp)) ,(caddr exp) ,@(cddddr exp))
      `(fdefun ,(cadr exp)
               ,(union (set-difference forced-vars (cadddr exp)) (caddr exp))
               ,(cadddr exp)
               ,@(cddddr exp) ) ) )
  
(install-binding-form
  'fdefun
  #'fdefun-matcher-fn
  #'fdefun-forcer-fn )

;; Next, install the ffdefun-expression into the system:
;;
;; for reference, the parameter positions in ffdefun call:
;;
;; (fdefun name forced-params body-1 body-2 ... body-n)
;; (car    cadr caddr         cdddr..................)
(defun ffdefun-matcher-fn (exp)
  (and (listp exp)
       (eq (car exp) 'ffdefun) ) )

(defun ffdefun-forcer-fn (forced-vars exp)
  `(fdefun ,(cadr exp) ,(union forced-vars (caddr exp)) ,(caddr exp) ,@(cddddr exp)) )
  
(install-binding-form
  'ffdefun
  #'ffdefun-matcher-fn
  #'ffdefun-forcer-fn )
