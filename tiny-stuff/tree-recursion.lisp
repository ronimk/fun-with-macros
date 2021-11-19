;;; The tree-recursion is a fun little macro that recurs through theoretically on any amount of lists given to it,
;;; performing a single, task to each leaf of the given trees.
;;;
;;; It's main object is to automate patterns of the form below:
;;;
;;; (if (or (atom t1) (atom t2) ... (atom tn)
;;;     (do-something-to t1 t2 ... tn)
;;;     (and (recur-on (first t1) (first t2) ... (first tn))
;;;          (recur-on (rest t1) (rest t2) ... (rest tn)) ) ))
;;;
;;; It also contains extra syntax, aiming to make it more convenient to use:
;;;
;;; Example (from PAIP, P. 155):
;;;
;;; Instead of typing:
;;;
;;; (defun simple-equal (x y)
;;;   (if (or (atom x) (atom y))
;;;       (eql x y)
;;;       (and (simple-equal (first x) (first y))
;;;            (simple-equal (rest x) (rest y)) ) ) )
;;;
;;; You can now simply type:
;;;
;;; (defun simple-equal (x y)
;;;  "Are x and y equal? (Don't check inside strings.)"
;;;  (tree-recursion (x y)
;;;    (leaf-case eql)
;;;    (recur-on simple-equal) ) )
;;;
;;; or even:
;;;
;;; (defun simple-equal (x y)
;;;  "Are x and y equal? (Don't check inside strings.)"
;;;  (tree-recursion (x y)
;;;    (recur-on simple-equal)
;;;    (leaf-case eql) ) )
;;;
;;; whichever you prefer...

(defmacro tree-recursion (trees (first-case-id first-case-f) (second-case-id second-case-f))
  (let ((leaf-check-forms (mapcar #'(lambda (tree-elem)
                                      `(atom ,tree-elem) )
                                  trees ))
        (first-forms (mapcar #'(lambda (tree-elem)
                                 `(first ,tree-elem) )
                             trees ))
        (rest-forms (mapcar #'(lambda (tree-elem)
                                `(rest ,tree-elem) )
                            trees ))
        (leaf-f (cond ((eql first-case-id 'leaf-case) first-case-f)
                           ((eql second-case-id 'leaf-case) second-case-f)
                           (t nil) ))
        (recur-f (cond ((eql first-case-id 'recur-on) first-case-f)
                           ((eql second-case-id 'recur-on) second-case-f)
                           (t nil) )) )
    (if (or (null leaf-f) (null recur-f))
        (error "invalid call: (tree-recursion ~S (~S ~S) (~S ~S)" trees first-case-id first-case-f  second-case-id second-case-f)
        `(if (or ,@leaf-check-forms)
             (,leaf-f ,@trees)
             (and (,recur-f ,@first-forms)
                  (,recur-f ,@rest-forms) ) ) ) ) )

; Example usage from PAIP - redefining the utility functions for ELIZA on P. 155:
(defun simple-equal (x y)
  "Are x and y equal? (Don't check inside strings.)"
  (tree-recursion (x y)
    (leaf-case eql)
    (recur-on simple-equal) ) )

(defun pat-match (pattern input)
  "Does pattern match input? Any variable can match anything."
  (if (variable-p pattern)
      t
      (tree-recursion (pattern input)
        (recur-on pat-match)
        (leaf-case eql) ) ) )

; While a simple macro, tree-recursion shows how to shift the work on list reductions to compile-phase, it was also really fun to
; implement a little bit of "domain specific syntax" (in the form of the leaf-case and recur-on keywords) into the macro and to work on
; getting it done properly, with a compile-time error checking on the special symbols used.
;
; Of course one could have simply used keyword parameters for the leaf-case and recur arguments, so that part was purely for me to "tune myself in"
; to some of the concepts I will need for my bachelor's thesis.
