





(define editor-repl-enabled 0)


(define editor-theme-repl-border-color '(list 0.04 0.14 0.04 1.0))
(define editor-theme-repl-background-color '(list 0.1 0.14 0.1 1.0))

(define editor-repl-buffer "")
(define editor-repl-result "")

(define editor-repl-cursor-x 0)
(define editor-repl-cursor-y 0)


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
    (set! editor-repl-enabled (if (= editor-repl-enabled 1) 0 1))
    )
   (
    (= editor-repl-enabled 0)
    ()
    )

   ( (> keycode 255)
    ()
   )
   ((= keycode 9)
    (do (
         (x 0 (+ x 1))
         (d 0 (editor-key-down-hook 32))
         )
        ((= x 3) ())
      )
    )
   ((= keycode 13)
        (if input-modifier-shift
        (begin

          (set! editor-repl-cursor-x 0)
          (let ((editor-fetch-buffer editor-repl-buffer))
            (set! editor-repl-result (append editor-repl-buffer " -> " "Syntax Error") )
            (set! editor-repl-buffer "")
            (set! editor-repl-result (append editor-fetch-buffer " -> " (str (eval-string editor-fetch-buffer (rootlet)))))
          )
        )

        (begin
          (set! editor-repl-cursor-y (+ 1 editor-repl-cursor-y))

          )
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
     ((= editor-repl-enabled 1)

      (let
          (
           (flashy-color (list (/ (+ 1 (sin (* 2 global-time))) 2) 0.4 0.4 0.6))
          )
      (begin
       (set-color '(0.2 0.2 0.2 1.0))
       (render-frame 0.0 0.0 global-window-x 200.0 4 (eval editor-theme-repl-border-color) (eval editor-theme-repl-background-color))
       (render-frame 0.0 196.0 global-window-x 30.0 4 (eval editor-theme-repl-border-color) (eval editor-theme-repl-background-color))
       (set-color '(0.0 1.0 1.0 1.0))
       (render-string-ttf editor-repl-buffer 6 6 16 '(0.87 1.0 0.92 1.0))
       (render-string-ttf editor-repl-result 6 204 16 '(0.9 1.0 0.93 1.0))
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
