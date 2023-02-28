;;;; for-each.lisp
;;;; Common Lisp already has a dolist-loop that can be used to iterate over lists.
;;;; However, it can only process one list at a time. To process multiple lists 
;;;; simultaneously, Common Lisp has powerful primitives, like mapc and loop. But 
;;;; those unfamiliar with mapping, or with the loop-primitive with its 
;;;; own powerful but complicated internal DSL, may find them difficult to use.
;;;; for-each is a simple looping construct over a multiple lists simultaneously.
;;;; The looping stops once any of the input lists are exhausted.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun make-binds&conds (vlpairs &optional (vb nil) (tb nil) (ncs nil))
   (if (null vlpairs)
       (values (nreverse vb) (nreverse tb) (nreverse ncs))
       (let* ((var-val-pair (first vlpairs))
              (var (first var-val-pair))
              (list (second var-val-pair))
              (g (gensym)) )
         (make-binds&conds (rest vlpairs)
                           (cons `(,var (first ,g)) vb)
                           (cons `(,g ,list (rest ,g)) tb)
                           (cons `(null ,g) ncs) ) ) ) ) )

(defmacro for-each ((&rest var-list-pairs) &body body)
 (multiple-value-bind (var-bindings tail-bindings null-conds)
                      (make-binds&conds var-list-pairs)
   `(do ,tail-bindings
        ((or ,@null-conds))
      (let ,var-bindings
        ,@body ) ) ) )

;; Simple examples:
(for-each ((e1 '(1 2 3 4 5 6)) (e2 '(3 3 3 3)) (e3 '(6 5 4 3 2 1)))
  (format t "~A " (+ e1 e2 e3)) )
;; => 10 10 10 10
(for-each ((e1 (append '(1 2 3) '(4 5 6)))
           (e2 (list 3 3 3 3))
           (e3 (first '((6 5 4 3 2 1) (1 2 3 4 5 6) (3 3 3 3)))) )
  (format t "~A " (+ e1 e2 e3)) )
;; => 10 10 10 10

;; The for-each macro builds an implicit do-call:
(macroexpand-1 '(for-each ((e1 '(1 2 3 4 5)) (e2 '(5 4 3 2 1)))
  (format t "~A~%" (+ e1 e2)) ) )
;; => (DO ((#:G466 '(1 2 3 4 5) (REST #:G466))
;;         (#:G467 '(5 4 3 2 1) (REST #:G467)))
;;        ((OR (NULL #:G466) (NULL #:G467)))
;;      (LET ((E1 (FIRST #:G466)) (E2 (FIRST #:G467)))
;;        (FORMAT T "~A~%" (+ E1 E2))))

;; the function make-binds&conds generates all the necessary parts for building the final do form.
;; It uses as accumulators the following variables:
;; * vb, which contains the variable bindings of the resulting let-form
;; * tb which contains the tail bindings of the given lists
;; * ncs which contains the termination conditions of the do-form.
;; The only only real complication comes from the fact that to avoid unintended variable capture 
;; and multiple evaluation of the list expressions, a new gensymed variable must be
;; created for each given list and the gensym variables used in the do-bindings/tail-bindings
;; must match the gensym variables used in the innremost let-bindings.
;; This is handled by the recursive make-binds&conds call:
;;     (let (...
;;           (g (gensym)) )
;;       (make-binds&conds (rest vlpairs)                  ;; the correct gensym will be matched in
;;                         (cons `(,var (first ,g)) vb)    ;; the variable bindings of the inermost let-form, and
;;                         (cons `(,g ,list (rest ,g)) tb) ;; in the do-bindings/tail bindings of the do-form
;;                         (cons `(null ,g) ncs) ) ) ) )   ;; technically, the termination conditions (the ncs - the null-condition checks)
                                                           ;; could be in any order whatsoever, but its natural to place them in matching order with the rest.
