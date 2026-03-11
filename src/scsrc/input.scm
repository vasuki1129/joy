
(define input-keycode-tilde 96)

(define key-down-hook-list '())
(define key-up-hook-list '())

(define (process-key-down keycode)
    (for-each (lambda (func) (func keycode)) key-down-hook-list)
)

(define (process-key-up keycode)
    (for-each (lambda (func) (func keycode)) key-up-hook-list)
)

(define (add-key-down-hook hook) (set! key-down-hook-list (cons hook key-down-hook-list)))
(define (add-key-up-hook hook) (set! key-up-hook-list (cons hook key-up-hook-list)))

(define (debug-print-keycode keycode) (begin (println (number->string keycode))))

(define input-modifier-shift #f)


(define (modifier-down-handler-hook keycode)
  (cond
   ((= keycode 1073742049)
    (set! input-modifier-shift #t)
    )
  )
)

(define (modifier-up-handler-hook keycode)
  (cond
   ((= keycode 1073742049)
    (set! input-modifier-shift #f)
   )
  )
)

(add-key-down-hook modifier-down-handler-hook)
(add-key-up-hook modifier-up-handler-hook)

(if (= global-debug-flag 1) (add-key-down-hook debug-print-keycode))
