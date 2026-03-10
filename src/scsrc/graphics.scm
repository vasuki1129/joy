

(define (render-string str x y xs ys)
  (do
      (
       (p (string->list str) (cdr p))
       (pos x (if (or (< c 32) (> c 127)) pos (+ pos xs)))
       (d 0 (if (or (< c 32) (> c 127)) (render-character (car p) pos y xs ys)))
       )
      ((null? p) 0)
  )
)


(define (render-string-literally str x y xs ys)
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

(define (render-string-formatted str x y xs ys)
  (do
      (
       (p (string->list str) (cdr p))
       (posx x (if (or (< (char->integer (car p)) 32) (> (char->integer (car p)) 127)) 0 (+ posx xs)))
       (posy y (if (char=? (car p) #\newline) (+ posy ys) posy))
       (d 0 (if (not (or (< (char->integer (car p)) 32) (> (char->integer (car p)) 127))) (render-character (car p) posx posy xs ys)))
      )
      ((null? p) 0)
  )
  )

(define (render-frame x y xs ys bw color-border color-background)
  (begin
    (set-color color-background)
    (fill-rect (list x y xs ys))
    (set-color color-border)
    (fill-rect (list x y xs bw))
    (fill-rect (list x y bw ys))
    (fill-rect (list (- xs bw) y bw ys))
    (fill-rect (list x (- ys bw) xs bw))
  )
)
