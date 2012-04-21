(setf (symbol-function 'r) #'read
      (symbol-function 'f) #'format
      (symbol-function 'q) #'find
      (symbol-function 's) #'set
      (symbol-function 'p) #'print
      (symbol-function 'm) #'mapcar
      (symbol-function 'n) #'not
      (symbol-function 'l) #'make-list
      (symbol-function 'a) #'car
      (symbol-function 'b) #'cadr)

(macrolet ((d (&rest r) `(dotimes ,@r))
	   (o (&rest r) `(do ,@r))
           (x (&rest r) `(pop ,@r))
	   (i (&rest r) `(if ,@r)))

  ; next line is 127 characters long
(d(j(r))(f t"~&Case #~D:"(1+ j))(r)(s'z(r))(d(a(r))(r)(p(o((g(m'r(l(r))))(p 1(*(x c)p))(c z(i(q(x c)g)(a c)(b c))))((n c)p)))))

) ; this one doesn't count, it belongs to macrolet
