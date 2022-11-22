;;; The Common Lisp port of Shriram Krishnamurthi's wonderful automata DSL
;;; (https://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/)
;;; this is the port for the final version of the paper with two major modifications:
;;;      1) accepting states can have paths to other states
;;;      2) wildcards are accepted in transformation rules
;;;           a rule label can now be a wildcard, in which case
;;;           that rule accepts any symbol in the stream.
;;;      iii) The user can input the wildcard as follows:
;;;              (automaton (:wildcard wildcard-symbol) start-state state-expressions)
;;;              Example:
;;;              (automaton (:wildcard ?) next-other
;;;                (next-other : (a -> next-a) (b -> next-b) (? -> next-other))
;;;                (next-a : (a -> end) (b -> next-b) (? -> next-other))
;;;                (next-b : (a -> next-a) (b -> end) (? -> next-other))
;;;                (end : accept (a -> next-a) (b -> next-b) (? -> doomed))
;;;                (doomed) )

; a few special values the language needs: 
(defparameter the-false-value 'false)
(defparameter the-truth-value 'true)
(defparameter the-accept-marker 'accept)
        
(defun transform-rules (rules keyword-list)
  "transforms a list of automaton rules into a case syntax:
   ({(<rule-label> -> <rule-dest>)}*)
     => ({((<rule-label>) (<rule-dest> (rest stream)))}*)
   Example:
     ((a -> more) (d -> more) (r -> end))
       => (((a) (more (rest stream)))
           ((d) (more (rest stream)))
           ((r) (end (rest stream)))
           (otherwise the-false-value) )

   The keyword list is currently used to recognize a wildcard symbol,
   but could be expanded to handle other things that alter
   the standard automata rule generation as well."
  (let ((the-wildcard (getf keyword-list :wildcard)) )
    (labels ((rule-label (rule)
               (first rule) )
             (rule-dest (rule)
               (third rule) )
             (wildcard-rule (rule)
                (eq (rule-label rule) the-wildcard) )
             (h (rules)
               (if (null rules)
                   (list (list 'otherwise 'the-false-value))
                   (let ((first-rule (first rules)))
                     (if (wildcard-rule first-rule)
                         (list `(otherwise (,(rule-dest first-rule) (rest stream))))
                         (cons `((,(rule-label first-rule))
                                 (,(rule-dest first-rule) (rest stream) ) )
                               (h (rest rules)) ) ) ) ) ) )
      (h rules) ) ) )
             
(defun make-state-fn (state-expr keyword-list)
  "Constructs a state function for the given state expression:
   Example:
     (more : (a -> more)
             (d -> more)
             (r -> end) )
       => (more (stream)
            (cond
              ((null stream) the-truth-value)
              (T
               (case (first stream)
                 ((a) (more (rest stream)))
                 ((d) (more (rest stream)))
                 ((r) (end (rest stream))) ) ) ) )
   
   If the state is an accepting state, it is of the form
   (label : accept), and thus has to be dealth with
   a bit differently."
   (let* ((accepting-state? (eq (third state-expr)
                                the-accept-marker ))
           (the-empty-value (if accepting-state?
                                'the-truth-value
                                'the-false-value ))
           (rules (if accepting-state? (cdddr state-expr) (cddr state-expr))) )
    `( ,(car state-expr) (stream)
       (cond
         ((null stream) ,the-empty-value)
         (T
          (case (first stream)
            ,@(transform-rules rules keyword-list)) ) ) ) ) )

(defmacro automaton (keyword-list start-state &rest state-expressions)
  "Constructs a Common Lisp representation of the given
   automaton description."
  (list 'labels
        (mapcar #'(lambda (state-expr)
                    (make-state-fn state-expr keyword-list) )
                state-expressions )
        `(function ,start-state) ) )
