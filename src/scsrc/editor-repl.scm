





(define editor-repl-enabled 1)
(define editor-repl-mode #t)

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
    (and (= keycode 91) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "{"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 93) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "}"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 50) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "@"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 51) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "#"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 52) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "$"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )

   (
    (and (= keycode 53) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "%"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )

   (
    (and (= keycode 54) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "^"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )

   (
    (and (= keycode 55) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "&"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )

   (
    (and (= keycode 56) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "*"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )

   (
    (and (= keycode 45) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "_"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 61) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "+"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 59) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer ":"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 44) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "<"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 46) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer ">"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 47) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "?"))
    (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
   )
   (
    (and (= keycode 39) input-modifier-shift)
    (set! editor-repl-buffer (append editor-repl-buffer "\""))
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

   ( (> keycode 126)
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
            (set! editor-repl-cursor-x 0)
            (set! editor-repl-cursor-y 0)
            (set! editor-repl-result (append editor-repl-buffer " -> " "Syntax Error") )
            (if editor-repl-mode (set! editor-repl-buffer ""))
            (set! editor-repl-result (append editor-fetch-buffer " -> " (str (eval-string editor-fetch-buffer (rootlet)))))
          )
        )

        (begin
          (set! editor-repl-cursor-y (+ 1 editor-repl-cursor-y))
          (set! editor-repl-cursor-x 0)
          (set! editor-repl-buffer
                (list->string (append (string->list editor-repl-buffer) (string->list "\n"))))
          )
        )


   )


   ((= keycode 8)
    (set! editor-repl-buffer (string-backspace editor-repl-buffer))
    (set! editor-repl-cursor-x (if (= editor-repl-cursor-x 0) 0 (- editor-repl-cursor-x 1)))
   )
   (else
    (if (> keycode 31)
      (begin
        (set! editor-repl-buffer (append editor-repl-buffer (string (integer->char keycode))))
        (set! editor-repl-cursor-x (+ 1 editor-repl-cursor-x))
      )
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
       (if (= (modulo (floor global-time) 2) 0) (render-string-ttf "|" (+ 5 (* editor-repl-cursor-x 8)) (+ 5 (* editor-repl-cursor-y 14)) 16 '(1.0 1.0 1.0 1.0)))
       )
      )
      )
     (else
     ())
    )
  )
)

(add-global-render-hook editor-render)
