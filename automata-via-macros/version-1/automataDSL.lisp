;;; The Common Lisp port of Shriram Krishnamurthi's wonderful automata DSL
;;; (https://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/)
;;; this is the port for the first - "buggy" - version of chapter 3 of his paper,
;;; developed in CLISP.

(defparameter the-false-value 'false)
(defparameter the-truth-value 'true)
        
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
  "Constructs a labels-syntax object from the given automaton
   state expression to be injected into a labels-syntax:
   Example:
     (more : (a -> more)
             (d -> more)
             (r -> end) )
       => ((more (stream)
             (cond
               ((null stream) the-truth-value)
               (T
                (case (first stream)
                  ((a) (more (rest stream)))
                  ((d) (more (rest stream)))
                  ((r) (end (rest stream)))
   The result can be turned into a proper labels-syntax
   simply by listing 'labels in the front."
  `( ,(car state-expr) (stream)
     (cond
       ((null stream) the-truth-value)
       (T
        (case (first stream)
          ,@(transform-rules (cddr state-expr))
          (otherwise the-false-value) ) ) ) ) )

(defmacro automaton (start-state &rest state-expressions)
  "Constructs a Common Lisp representation of the given
   automaton description."
  (list 'labels
        (mapcar #'make-state-fn state-expressions)
        `(function ,start-state) ) )
