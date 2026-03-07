
(define (println x) (begin
                        (print x)
                        (print "\n")
                        ))

(define (str obj) (object->string obj))


(define (first val) (car val))
(define (last val)
  (do
    (
     (x val (cdr x))
     (out () (car x))
    )
    ((null? x) out)
  )
)


(define (nth val n)
  (do
    (
     (x val (cdr x))
     (ctr 0 (+ ctr 1))
    )
    ((= ctr n) (car x))
  )
)
