(define (x val) (car val))
(define (y val) (car (cdr val)))
(define (z val) (car (cdr (cdr val))))

(define (dot a b) (do (
                       (ap a (cdr ap))
                       (bp b (cdr bp))
                       (out 0 (+ out (* (car ap) (car bp))))
                       )
                      ((or (null? ap) (null? bp)) out)))

(define (addvec a b) (do (
                          (ap a (cdr ap))
                          (bp b (cdr bp))
                          (out '() (cons (+ (car ap) (car bp)) out))
                          )
                          ((or (null? ap) (null? bp)) out)
                        )
  )
