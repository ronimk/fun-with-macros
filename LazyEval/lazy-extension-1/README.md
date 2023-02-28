<pre>
To make the delay system more useable in practice, in this first phase it is augmented with the 
following macros:

* with-forced-vars
* flambda  (forced-lambda)
* fdefun   (forced-defun)
* fflambda (fully-forced-lambda)
* ffdefun  (fully-forced-defun)
 
The  following example code show how these macros can be used. The variables that 
are in bold and italic will be automatically forced in the resulting expression: <br /> <br />

(let ((a (delay 1)) (b (delay 1)) (c 1) (d 1)
      (e (delay 1)) (x (delay 1)) (z (delay 1)) )
  (with-forced-vars (a b e x) <br />
    (+ (funcall (flambda (x) :repeat <br />
                  (fdefun z+x (z) () (+ **_z_** **_x_**)) <br />
                    (z+x) ) <br />
             (delay (* 3 **_x_**)) ) ;; => 4 <br />
       (let ((b ((lambda (x) (+ **_a_** **_b_** x c)) **_e_**)) ;; b := 4 <br />
              e )                             ;; e := NIL <br />
         (+ b c **_x_** d ;; => 4+1+1+1 <br />
            (if (null e) 0 1) ) ) ;; +0 = 7 <br />
       **_b_** **_e_** ) ) ) ;; +1+1 <br />
=>	13  ;; 4 + 7 + 1 + 1 = 13 <br /> <br />

As can be seen, nested binding forms, like lambda and let, inside these macros will override the automatic forcing mechanism.
Currently only nested lambda- and let-forms are installed into the system to override the automatic forcing mechanism, nothing
prevents users from installing other Common Lisp primitives, or even completely new binding forms created by other
macro programmers.

For example, if a user has created the following notations for writing functions:

(my-func -> x y z        ; define a new function named my-func <br />
  (body-expression-1     ; note that the arrow identifier is not <br />
   body-expression-2     ; at the beginning of the list. <br />
   body-expression-3 ) ) <br /> <br />

(-> x y z                ; create a new anonymous function <br />
  (body-expression-1 <br />
   body-expression-2 <br />
   body-expression-3 ) ) <br />

They can be installed into the system with the function install-binding-form.

This overshadowing behaviour of nested binding form variables is, strictly speaking, unnecessary.
nothing bad happens with forcing non-delayed values, other than perhaps unnecessary and time-consuming,
but otherwise harmless nested force calls.

But any nested binding the user wants the system to recognize, must be specially processed in some way.
Otherwise, the following expression, for example: <br />

(flambda (x) (let ((x (+ x 1)) x)) <br />

Would be macro-expanded into the expression below, which would clearly produce errors when compiling or interpreting: <br />

(flambda (x) (let (((force x) (+ (force x) 1)) (force x))) <br />
</pre>
