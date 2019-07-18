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
(defparameter the-generic-rule-type 'generic)
(defparameter the-wildcard-rule-type 'wildcard)
(defparameter the-choice-rule-type 'choice)

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
                    the-generic-rule-type ) )
             (get-rule-transformer (rule-type)
               (getf rule-transformers rule-type) )
             (dispatch (msg)
               (case msg
                 ((get-rule-type) #'get-rule-type)
                 ((get-rule-transformer) #'get-rule-transformer)
                 ((install-rule) #'install-rule) ) ) )
      #'dispatch ) ) )

; rule-transformation-ds contains the RTDS:
(setf rule-transformation-ds (init-rules))

;; The rule transformation data structure interface:
(defun get-rule-type (rule keyword-list)
" get-rule-type finds the type of the given rule
  Examples:
    1. (get-rule-type (* -> red) '(:wildcard *)) => 'wildcard
    2. (get-rule-type (y -> yellow) '(:wildcard *)) => 'generic"
  (funcall (funcall rule-transformation-ds 'get-rule-type) rule keyword-list) )


(defun lookup-rule-transformer (rule-type)
" lookup-rule-transformer finds the transformer of the given rule's type."
  (funcall (funcall rule-transformation-ds 'get-rule-transformer) rule-type) )

(defun transform-rule (rule keyword-list rest)
" transform-rule is the primary interface function that can be used to handle everything
  necessary to transform a automaton rule(set) into a proper LISP code."
  (funcall (lookup-rule-transformer (get-rule-type rule keyword-list)) rule rest) )


(defun rule-installer (rule-type rule-inferer rule-transformer)
" A helper function to insall a new rule transformation objects to the system.
  An RTO consists of three things:
 
    1. rule-type, which is a non-NIL symbol that uniquely identifies the type
       of the given RTO.
 
    2. rule-inferer is a predicate function that is used to check
       whether a rule is of a certain type.
       The rule-inferer predicate takes in a rule and a keyword list
       and returns either the rule type or NIL if the rule does not match.
 
    3. rule-transformer is a function that handles transforming a given
       rule of a matching rule type into LISP code AND combining it
       with the rest of the rule transformations.
       rule-transformer functions take in the current rule to be combined
       and the rest of transformed rules that follow the current rule
 
  The main rule transformer (transform-rules) delegates the work of combining
  transformed rules to individual rule transform objects by using a continuation
  passing style. This allows more flexibility, better handling and easier
  maintenance in installing and working with RTOs."
  (funcall (funcall rule-transformation-ds 'install-rule)
           rule-type
           rule-inferer
           rule-transformer ) )

;; Install code for the rules:
; Install code for the wildcard rule:
(rule-installer the-wildcard-rule-type
                #'(lambda (rule keyword-list)
                    (aand (getf keyword-list :wildcard)
                    (eq (first rule) it) ) ) 
                #'(lambda (rule rest)
                    (list `(otherwise (,(third rule) (rest stream)))) ) )


; Install code for the choice rule:
; To simplify things, choice can only be used for accepting
; single predefined symbols; no wildcards can be used nor
; can choices be nested nor any other special rules used.
; (It seems to make little sense to combine
;  any special rules with the choice rule anyway...)
(rule-installer the-choice-rule-type
                #'(lambda (rule keyword-list)
                    (consp (first rule)) )
                #'(lambda (rule rest)
                    (append (reduce #'(lambda (r1 rest)
                                      (funcall (lookup-rule-transformer the-generic-rule-type) r1 rest) )
                                 (choice->ruleset rule)
                                 :from-end t
                                 :initial-value nil)
                          rest ) ) )

; choice->ruleset transforms a choice rule into a set of generic rules.
; Example:
; (coice->ruleset '((r y g) -> red))
; => ((r -> red) (y -> red) (g -> red))
(defun choice->ruleset (choice-rule)
  (let ((rule-dest (caddr choice-rule)))
    (mapcar #'(lambda (rule-sym)
                (list rule-sym '-> rule-dest) )
           (first choice-rule) ) ) )

; Install code for the generic rule:
(rule-installer the-generic-rule-type
                #'(lambda (rule keyword-list) nil) ; We do not want a rule inference function for generic rules.
                #'(lambda (rule rest)              ; get-rule-type handles that for us.
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
