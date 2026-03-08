
(define global-debug-flag 1)



(define global-init (list))
(define global-update (list))
(define global-render (list))

(define (add-global-render-hook hook) (begin
                                        (display "adding hook")
                                        (newline)
                                        (set! global-render (cons hook global-render))
                                        ))

(define editor-loaded-scheme-file '())


(define test-renderfunc (lambda () (begin
                                     (set-color '(1.0 1.0 1.0 1.0))
                                     (render-string "hello" 0 0 16 16))

                                     )
                                     )


(add-global-render-hook test-renderfunc)

(define global-time 0.0)
(define global-delta 0.0)

(define (global-update-process)  (for-each (lambda (func) (func)) global-update))
(define (global-render-process)  (for-each (lambda (func) (func)) global-render))

(print global-render)
