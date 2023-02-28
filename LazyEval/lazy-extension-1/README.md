
<pre>
To make the delay system more useable in practice, in this first phase it is augmented with the 
following macros:

* with-forced-vars
* flambda  (forced-lambda)
* fdefun   (forced-defun)
* fflambda (fully-forced-lambda)
* ffdefun  (fully-forced-defun)
 
The  following example code show how these macros can be used:

(let ((a (delay 1)) (b (delay 1)) (c 1) (d 1) 
      (e (delay 1)) (x (delay 1)) (z (delay 1)) )
  (with-forced-vars (a b e x)
    (+ (funcall (flambda (x) :repeat
                  (fdefun z+x (z) () (+ z x))
                    (z+x) )
                  (delay (* 3 x)) ) ;; => 4
       (let ((b ((lambda (x) (+ a b x c)) e)) ;; b := 4
              e )                                               ;; e := NIL
         (+ b c x d ;; => 4+1+1+1
           (if (null e) 0 1) ) ) ;; +0 = 7
       b e ) ) ) ;; +1+1
=>	13  ;; 4 + 7 + 1 + 1 = 13

The above example equals the following code:

(let ((a (delay 1)) (b (delay 1)) (c 1) (d 1) 
      (e (delay 1)) (x (delay 1)) (z (delay 1)) )
    (+ (funcall (lambda (x)
                  (defun z+x () (+ (force z) (force x)))
                    (z+x) )
                  (delay (* 3 (force x))) ) ;; => 4
       (let ((b ((lambda (x) (+ (force a) (force b) x c)) (force e)))
              e )
         (+ b c (force x) d
           (if (null e) 0 1) ) )
       (force b) (force e) ) ) )

As can be seen, nested binding forms, like lambda and let, inside these macros will override the automatic forcing mechanism.
Currently only nested lambda- and let-forms are installed into the system to override the automatic forcing mechanism, nothing
prevents users from installing other Common Lisp primitives, or even completely new binding forms created by other
macro programmers.

For example, if a user has created the following notations for writing functions:

(my-func -> x y z        ; define a new function named my-func
  (body-expression-1     ; note that the arrow identifier is not
   body-expression-2     ; at the beginning of the list.
   body-expression-3 ) )

(-> x y z                ; create a new anonymous function
  (body-expression-1
   body-expression-2
   body-expression-3 ) )

They can be installed into the system with the function install-binding-form.

This overshadowing behaviour of nested binding form variables is, strictly speaking, unnecessary.
nothing bad happens with forcing non-delayed values, other than perhaps unnecessary and time-consuming,
but otherwise harmless nested force calls.

But any nested binding the user wants the system to recognize, must be specially processed in some way.
Otherwise, the following expression, for example:

(flambda (x) (let ((x (+ x 1)) x))

Would be macro-expanded into the expression below, which would clearly produce errors when compiling or interpreting: <br />

(flambda (x) (let (((force x) (+ (force x) 1)) (force x)))

TODO: add the syntax of the language here
</pre>
