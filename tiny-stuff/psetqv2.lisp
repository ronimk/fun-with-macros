;;;; My implementation of Common Lisp PSETQ v.2
;;;;
;;;; it is a parallel version of SETQ, meaning that
;;;; all new values of PSETQ variables are evaluated
;;;; first, before any assignment operation.

;; The my-psetq -macro transform psetq-expressions of the form:
;; (my-psetq {symbol expr}*)
;; into the form:
;; ((lambda ({gensym-var}*)
;;    {(setq gensym-var value)}* )
;;  {expr}* )
;;
;; Example:
;;
;; > (setq x 1 y 2 z 3)
;; > x => 1
;; > y => 2
;; > z => 3
;;
;; > (my-psetq x (1+ y) y (1+ z) z (1+ x))
;; > x => 3
;; > y => 4
;; > z => 2
;;
;; > (macroexpand '(my-psetq x (1+ y) y (1+ z) z (1+ x)))
;; => ((lambda (#:G2 #:G1 #:G0)
;;       (setq z #:G2 y #:G1 x #:G0) )
;;     (1+ x) (1+ z) (1+ y) ) )
;;

(defmacro my-psetq (&rest args)
  (multiple-value-bind (varpairs vals) (collect-vars&vals args)
    (let ((setq-pairs (apply #'append varpairs)))
      `((lambda ,(mapcar #'(lambda (varpair)
                             (cadr varpair) )
                         varpairs )
          (setq ,@setq-pairs))
        ,@vals ) ) ) )
      
(defun collect-vars&vals (psetq-args)
  "Collects the psteq arguments and separates them into two lists:
    a    ({(var (gensym))}*) list and
    an   ({expr}*) list "
  (labels ((h (vars vals args)
           (cond ((null args) (values vars vals))
                 (T
                  (h (cons (list (car args)
                                 (gensym) )
                           vars )
                     (cons (cadr args) vals)
                     (cddr args) ) ) ) ))
    (h nil nil psetq-args) ) )
