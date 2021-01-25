; From the tutorial "Fundamentals of CLOS" by Nick Levine
; Exercise 2: Implemented with-slots by symbol-macrolet.
;
; (See CLHS for more details on the with-slots -macro:
; http://www.lispworks.com/documentation/HyperSpec/Body/m_w_slts.htm)

(defmacro my-with-slots (slot-entries instance-form &body body)
  (let* ((instance instance-form)                                                ; according to CLHS, the supplied instance-form should be evaluated. At the moment, this is broken
         (symbol-expansion-forms                                                 ; The symbol-macrolet's symbol-expansion-forms are computed beforehand
           (mapcar (lambda (slot-entry)                                          ; To compute the symbol-expansion-forms each slot-entry given will be
                     (let ((var (if (symbolp slot-entry)                         ; examined based on whether it is of the form 'symbol' or of the form
                                    slot-entry                                   ; '(var symbol)'
                                    (car slot-entry) ))
                           (slot-name (if (symbolp slot-entry)
                                          slot-entry
                                          (cadr slot-entry) )) )
                       (list var (make-accessor-form instance slot-name)) ) )
             slot-entries ) ) )
    `(symbol-macrolet ,symbol-expansion-forms ,@body) ) )

; make-accessor-form creates lists of the form:
; '(slot-value [instance] [slot-name])
; which are used within my-with-slots to bind slot-variables to their accessor forms.
(defun make-accessor-form (instance slot-name)
  (list 'slot-value instance (list 'quote slot-name)) )
  
; A use case (adaptation of the CLHS example):
; (defclass thing ()
;           ((x :initarg :x :accessor thing-x)
;            (y :initarg :y :accessor thing-y)))
; =>  #<STANDARD-CLASS THING 250020173>
; (defmethod (setf thing-x) :before (new-x (thing thing))
;   (format t "~&Changing X from ~D to ~D in ~S.~%"
;           (thing-x thing) new-x thing) )
; (setq thing (make-instance 'thing :x 0 :y 1))
; =>  #<THING 62310540>
; (my-with-slots (x y) thing (incf x) (incf y))
; =>  2
; (values (thing-x thing) (thing-y thing))
; =>  1, 2
; (setq thing1 (make-instance 'thing :x 1 :y 2))
; =>  #<THING 43135676>
; (setq thing2 (make-instance 'thing :x 7 :y 8))
; =>  #<THING 43147374>
; (my-with-slots ((x1 x) (y1 y))
;             thing1
;   (my-with-slots ((x2 x) (y2 y))
;               thing2
;     (list (list x1 (thing-x thing1) y1 (thing-y thing1)
;                 x2 (thing-x thing2) y2 (thing-y thing2))
;           (setq x1 (+ y1 x2))
;           (list x1 (thing-x thing1) y1 (thing-y thing1)
;                 x2 (thing-x thing2) y2 (thing-y thing2))
;           (setf (thing-x thing2) (list x1))
;           (list x1 (thing-x thing1) y1 (thing-y thing1)
;                 x2 (thing-x thing2) y2 (thing-y thing2)))))
; >>  Changing X from 7 to (9) in #<THING 43147374>.
; =>  ((1 1 2 2 7 7 8 8)
;      9
;      (9 9 2 2 7 7 8 8) 
;      (9)
;      (9 9 2 2 (9) (9) 8 8))
