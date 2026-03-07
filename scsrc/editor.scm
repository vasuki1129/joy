(define editor-shown 1)

(define (editor-process)
  (begin

  )
)

(define (editor-render)
  (begin
    (cond
     ((= editor-shown 1)
     (begin
       (set-color '(0.2 0.2 0.2 1.0))
       (fill-rect '(0.0 0.0 800.0 200.0))
     ))
     (else
     ())

    )
  )
)

(add-global-render-hook editor-render)
