;; This file contains a slightly modified version of the lazy evaluation 
;; system in On Lisp. The only difference is that after forcing a delayed value, it’s 
;; delay-closure will be set to nil, to help the garbage collection get rid of functions 
;; that are no longer needed:

(defconstant unforced (gensym))

(defstruct delay forced closure)

(defmacro delay (exp)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             (lambda ()
               (setf (delay-forced ,self) ,exp)
               (setf (delay-closure ,self) nil)
               (delay-forced ,self) ) )
       ,self ) ) )

(defun force-exp-p (exp)
 (and (listp exp)
      (equal (first exp) 'force) ) )
 
(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (delay-closure x))
          (delay-forced x) )
      x ) )
