(define global-debug-flag 1)
(define global-init (list))
(define global-update (list))
(define global-render (list))


(define (add-global-render-hook hook) (begin
                                        (display "adding hook")
                                        (newline)
                                        (set! global-render (cons hook global-render))
                                        ))

(define (add-global-update-hook hook) (begin
                                        (display "adding update hook")
                                        (newline)
                                        (set! global-update (cons hook global-update))))

(define (add-global-init-hook hook) (begin
                                     (display "adding init hook")
                                     (newline)
                                     (set! global-init (cons hook global-init))))

(define global-time 0.0)
(define global-delta 0.0)

(define (global-init-process) (for-each (lambda (func) (func)) global-init))
(define (global-update-process)  (for-each (lambda (func) (func)) global-update))
(define (global-render-process)  (for-each (lambda (func) (func)) global-render))

(print global-render)
