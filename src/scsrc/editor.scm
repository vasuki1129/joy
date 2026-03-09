(define editor-shown 1)
(define editor-loaded-scheme-file '())

(define (editor-process)
  (begin

  )
)

(define editor-repl-buffer (list))

(define (editor-key-down-hook keycode)
  (if (> keycode 255)
      ()
      (set! editor-repl-buffer (append editor-repl-buffer (list (integer->char keycode))))
  )
)

(add-key-down-hook editor-key-down-hook)

(define (editor-render)
  (begin
    (cond
     ((= editor-shown 1)
     (begin
       (set-color '(0.2 0.2 0.2 1.0))
       (fill-rect '(0.0 0.0 800.0 200.0))
       (set-color '(1.0 1.0 1.0 1.0))
       (render-string (list->string editor-repl-buffer) 0 0 16 16)
       (render-string (number->string global-time) 0 16 16 16)
     ))
     (else
     ())

    )
  )
)

(add-global-render-hook editor-render)
