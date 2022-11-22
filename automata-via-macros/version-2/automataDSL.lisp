;;; The Common Lisp port of Shriram Krishnamurthi's wonderful automata DSL
;;; (https://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/)
;;; this is the port for the final version of the paper.

; a few special values the language needs: 
(defparameter the-false-value 'false)
(defparameter the-truth-value 'true)
(defparameter the-accept-marker 'accept)
        
(defun transform-rules (rules)
  "transforms a list of automaton rules into a case syntax:
   ({(<rule-label> -> <rule-dest>)}*)
     => ({((<rule-label>) (<rule-dest> (rest stream)))}*)
   Example:
     ((a -> more) (d -> more) (r -> end))
       => (((a) (more (rest stream)))
           ((d) (more (rest stream)))
           ((r) (end (rest stream))) )
   "
  (labels ((rule-label (rule)
             (first rule) )
           (rule-dest (rule)
             (third rule) ))
    (mapcar #'(lambda (rule)
               `((,(rule-label rule)) (,(rule-dest rule) (rest stream))) )
            rules ) ) )
             
(defun make-state-fn (state-expr)
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
                                'the-false-value )) )
    `( ,(car state-expr) (stream)
       (cond
         ((null stream) ,the-empty-value)
         (T
          ,(if accepting-state?
               'the-false-value
               `(case (first stream)
                  ,@(transform-rules (cddr state-expr))
                  (otherwise the-false-value) ) ) ) ) ) ) )

(defmacro automaton (start-state &rest state-expressions)
  "Constructs a Common Lisp representation of the given
   automaton description."
  (list 'labels
        (mapcar #'make-state-fn state-expressions)
        `(function ,start-state) ) )
