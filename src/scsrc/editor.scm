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
       (render-frame 0.0 0.0 global-window-x 200.0 4 '(0.4 0.4 0.4 1.0) '(0.2 0.2 0.2 1.0))
       (set-color '(1.0 1.0 1.0 1.0))
       (render-string-formatted (object->string (/ 1 global-delta)) 0 0 12 12)
     ))
     (else
     ())
    )
  )
)

(add-global-render-hook editor-render)
