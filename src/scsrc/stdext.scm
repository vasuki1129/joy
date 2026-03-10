
(define (println x) (begin
                        (print x)
                        (print "\n")
                        ))

(define (str obj) (object->string obj))

(define (first val) (car val))

(define (last val)
  (list-ref val (- (length val) 1))
)

(define (nth val n)
  (list-ref val n))
