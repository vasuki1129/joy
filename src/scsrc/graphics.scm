

(define (render-string str x y xs ys)
  (do
      (
       (p (string->list str) (cdr p))
       (pos x (+ pos xs))
       (d 0 (render-character (car p) pos y xs ys))
       )
      ((null? p) 0)
    )
  )

(define (render-string-wrapped str x y xs ys line-length)
  (do
      (
       (p (string->list str) (cdr p))
       (pos-x x (modulo (+ pos-x  xs) (* line-length xs)))
       (pos-y y (if (= (modulo (+ pos-x  xs) (* line-length xs)) 0) (+ pos-y ys) pos-y))
       (d 0 (render-character (car p) pos-x pos-y xs ys))
      )
      (
       (null? p) ()
      )
    )
)
