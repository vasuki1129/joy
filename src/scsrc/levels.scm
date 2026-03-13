
(define (make-gameobject)
  (list
   "gameobject"
   '(0.0 0.0)
   0.0
   '(1.0 1.0)
   ()
   ()
   ()
   ()
  )
)

(define (gameobject-add-child obj child)
  (list
   (list-ref obj 0)
   (list-ref obj 1)
   (list-ref obj 2)
   (list-ref obj 3)
   (append (list-ref obj 4) (list child))
   (list-ref obj 5)
   (list-ref obj 6)
   (list-ref obj 7)
  )
)

(define (gameobject-set-name obj name)
  (list
   name
   (list-ref obj 1)
   (list-ref obj 2)
   (list-ref obj 3)
   (list-ref obj 4)
   (list-ref obj 5)
   (list-ref obj 6)
   (list-ref obj 7)
  )
)
