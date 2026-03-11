(define editor-shown 0)
(define editor-loaded-scheme-file '())

(define editor-theme-repl-border-color '(list 0.2 0.2 0.2 1.0))
(define editor-theme-repl-background-color '(list 0.3 0.3 0.3 1.0))

(define (editor-process)
  (begin

  )
)

(define editor-repl-buffer "")
(define editor-repl-result "")
(define editor-fetch-buffer "")

(define editor-repl-cursor-x 0)
(define editor-repl-cursor-y 0)

(define (catch-eval str)
  (eval-string str (rootlet))
)

(define (editor-key-down-hook keycode)

  (cond
   (
    (and (= keycode 48) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer ")"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 49) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "!"))

      (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
    )
   (
    (and (= keycode 57) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "("))

      (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
    )
   (
    (= keycode 96)
    (set! editor-shown (if (= editor-shown 1) 0 1))
    )
   (
    (= editor-shown 0)
    ()
    )

   ((> keycode 255)
    ()
   )
   ((= keycode 13)
        (begin

          (set! editor-repl-cursor-x 0)
          (set! editor-fetch-buffer editor-repl-buffer)
          (set! editor-repl-result (append editor-repl-buffer " -> " "Syntax Error") )
          (set! editor-repl-buffer "")
          (set! editor-repl-result (append editor-fetch-buffer " -> " (str (catch-eval editor-fetch-buffer))))
        )
   )


   ((= keycode 8)
    (set! editor-repl-buffer (string-backspace editor-repl-buffer))
    (set! editor-repl-cursor-x (if (= editor-repl-cursor-x 0) 0 (- editor-repl-cursor-x 1)))
   )
   (else
    (begin
      (set! editor-repl-buffer (append editor-repl-buffer (string (integer->char keycode))))
      (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
    )
   )
  )
)

(add-key-down-hook editor-key-down-hook)

(define (editor-render)
  (begin
    (cond
     ((= editor-shown 1)

      (let
          (
           (flashy-color (list (/ (+ 1 (sin (* 2 global-time))) 2) 0.4 0.4 0.6))
          )
      (begin
       (set-color '(0.2 0.2 0.2 1.0))
       (render-frame 0.0 0.0 global-window-x 200.0 1 (eval editor-theme-repl-border-color) (eval editor-theme-repl-background-color))
       (render-frame 0.0 196.0 global-window-x 30.0 1 (eval editor-theme-repl-border-color) (eval editor-theme-repl-background-color))
       (set-color '(0.0 1.0 1.0 1.0))
       (render-string-ttf editor-repl-buffer 6 6 16 '(1.0 1.0 1.0 1.0))
       (render-string-ttf editor-repl-result 6 202 16 '(1.0 1.0 1.0 1.0))
       (if (= (modulo (floor global-time) 2) 0) (render-string-ttf "|" (+ 5 (* editor-repl-cursor-x 8)) 6 16 '(1.0 1.0 1.0 1.0)))
       )
      )
      )
     (else
     ())
    )
  )
)

(add-global-render-hook editor-render)
