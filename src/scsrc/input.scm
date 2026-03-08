
(define input-keycode-tilde 96)

(define key-down-hook-list '())
(define (process-key-down keycode)
    (for-each (lambda (func) (func keycode)) key-down-hook-list)
)
(define (add-key-down-hook hook) (set! key-down-hook-list (cons hook key-down-hook-list)))

(define (debug-print-keycode keycode) (begin (println (number->string keycode))))



(if (= global-debug-flag 1) (add-key-down-hook debug-print-keycode))
