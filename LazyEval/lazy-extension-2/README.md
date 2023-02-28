<pre>
Where lazy-extension-1 used quite an awkward syntax for flambda and fdefun,
and therefore needed additional fflambda and ffdefun primitives to make it more conveninet
to use the language, the lazy-extension-2 uses a syntax similar to Doug Hyote's defmacro-bang:

(let ((b (delay 1)) (e (delay 1)) (x (delay 1)))
  (fdefun my-f (f#a f#c d)
    (+ a (let ((b (funcall (flambda (f#x d) (+ x c d)) e d))
                e )
           (+ b c d (if (null e) 0 1) (force x)) )
       (force b) c d (force e) (force x) ) )
  (my-f b e 1) )
  
In flambda and fdefun, every variable that contains the prefix f#, will be forced within the body, unless
some other nested binding form binds them. For example, the above code example will be equal to the one below:

(let ((b (delay 1)) (e (delay 1)) (x (delay 1)))
  (defun my-f (a c d)
    (+ (force a) (let ((b (funcall (lambda (x d) (+ (force x) (force c) d)) e d))
                e )
           (+ b (force c) d (if (null e) 0 1) (force x)) )
       (force b) (force c) d (force e) (force x) ) )
  (my-f b e 1) )

As an additional benefit, with the lazy-extension-2 fflambda and ffdefun are no longer needed.

The only semantic difference between the new code and the old code is the fact that there no longer exists
a convenient way to include forcable variables that are not also function parameters. However, the
following pattern can be used to work around this when necessary:

(flambda param-list
  (with-forced-params (non-parametric-forcable-var-list)
    body) )
    
To eliminate the above pattern, if necessary, the following macro can be utilized:

(defmacro fv-flambda (non-params params &rest body)
  `(flambda ,params
     (with-forced-vars ,non-params ,@body) ) )

which would require the installing of the fv-lambda -form into the system.
</pre>
