

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
