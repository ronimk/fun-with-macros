;; INCOMPLETE:
;; More documentation needed and a choice rule needs to be added.

;;; The Common Lisp port of Shriram Krishnamurthi's wonderful automata DSL
;;; (https://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/)
;;; this is the port for the final version of the paper.

;; Note that the parenthesis indentation has been adopted
;; from the book "Lisp in Small Pieces". While it may seem
;; a bit odd at first to have spaces between trailing
;; parentheses whenever the matching opening parentese is not
;; on the same line as the last closing parenthese, I have
;; found that style so useful that it has become a habit for
;; me to write lisp code in that way.

; a few special values the language needs: 
(defparameter the-false-value 'false)
(defparameter the-truth-value 'true)
(defparameter the-accept-marker 'accept)

;; The rule-transformation data structure functionality:

; First, some anaphoric helper macros:
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form) ) )

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args)))) ) )

; init-rules creates the a new rule transformation data structure for installing
; new rules to the system and using them.
; It creates a new object that can be used to install new rules, find a rule type
; and get a rule transformer.
(defun init-rules ()
  (let ((rule-inferers nil)
        (rule-transformers nil) )
    (labels ((install-rule (rule-type rule-inferer rule-transformer)
               (push #'(lambda (rule keyword-list)
                         (if (funcall rule-inferer rule keyword-list)
                              rule-type ) )
                     rule-inferers )
               (setf (getf rule-transformers rule-type) rule-transformer) )
             (get-rule-type (rule keyword-list)
               (aif (some #'(lambda (rif)
                              (funcall rif rule keyword-list) )
                       rule-inferers )
                    it
                    'generic ) )
             (get-rule-transformer (rule-type)
               (getf rule-transformers rule-type) )
             (dispatch (msg)
               (case msg
                 ((get-rule-type) #'get-rule-type)
                 ((get-rule-transformer) #'get-rule-transformer)
                 ((install-rule) #'install-rule) ) ) )
      #'dispatch ) ) )

; rule-transformation-ds holds the RTDS
(setf rule-transformation-ds (init-rules))

;; The rule transformation data structure interface:
; get-rule-type finds the type of the given rule
; Examples:
;   1. (get-rule-type (* -> red) '(:wildcard *)) => 'wildcard
;   2. (get-rule-type (y -> yellow) '(:wildcard *)) => 'generic
(defun get-rule-type (rule keyword-list)
  (funcall (funcall rule-transformation-ds 'get-rule-type) rule keyword-list) )

; lookup-rule-transformer finds the transformer of the given rule's type:
(defun lookup-rule-transformer (rule-type)
  (funcall (funcall rule-transformation-ds 'get-rule-transformer) rule-type) )

; transform-rule is the primary interface function that can be used to handle everything
; necessary to transform a automaton rule(set) into a proper LISP code.
(defun transform-rule (rule keyword-list rest)
  (funcall (lookup-rule-transformer (get-rule-type rule keyword-list)) rule rest) )

;; Install code for the rules:
; Install code for the wildcard rule:
(funcall (funcall rule-transformation-ds 'install-rule)
         'wildcard
         #'(lambda (rule keyword-list)
           (aand (getf keyword-list :wildcard)
                 (eq (first rule) it) ) ) 
         #'(lambda (rule rest)
            (list `(otherwise (,(third rule) (rest stream)))) ) )

;Install code for the generic rule:

(funcall (funcall rule-transformation-ds 'install-rule)
         'generic
         #'(lambda (rule keyword-list) nil) ; We do not want a rule inference function for generic rules.
         #'(lambda (rule rest)
             (cons `((,(first rule))
                     (,(third rule) (rest stream)) )
                   rest ) ) )

; The main rule transformer function:
(defun transform-rules (rules keyword-list)
  "transforms a list of automaton rules into a case syntax:
   ({(<rule-label> -> <rule-dest>)}*)
     => ({((<rule-label>) (<rule-dest> (rest stream)))}*)
   Example:
     ((a -> more) (d -> more) (r -> end))
       => (((a) (more (rest stream)))
           ((d) (more (rest stream)))
           ((r) (end (rest stream))) )

   The keyword list is currently used to recognize a wildcard symbol,
   but could be expanded to handle other things that alter
   the standard automata rule generation as well."
  (labels ((h (rules cont)
             (if (null rules)
                 (funcall cont (list (list 'otherwise 'the-false-value)))
                 (h (rest rules) #'(lambda (rest-transformations)
                                     (funcall cont (transform-rule (first rules) keyword-list rest-transformations)) )) ) ))
    (h rules #'(lambda (x) x)) ) )

             
(defun make-state-fn (state-expr keyword-list)
  "Constructs a labels-syntax object from the given automaton
   state expression to be injected into a labels-syntax:
   Example:
     (more : (a -> more)
             (d -> more)
             (r -> end) )
       => (more (stream)
            (cond
              ((null stream) the-false-value)
              (T
               (case (first stream)
                 ((a) (more (rest stream)))
                 ((d) (more (rest stream)))
                 ((r) (end (rest stream))) ) ) ) ))
   
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
