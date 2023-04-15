#lang racket/base
(require racket/class racket/port)
(provide buffer%)

(define buffer%
  (class object%
    (super-new)

    (init-field size)

    (define buffer (make-bytes size))
    
    (define current-input (make-parameter (current-input-port)))
    (define current-output (make-parameter (current-output-port)))

    (define (set-input port) (current-input port))
    (define (set-output port) (current-output port))

    (define (read (handler #f))
      (sync (if handler (handle-evt (read-bytes!-evt buffer (current-input)) (lambda (n) (handler n buffer))) (read-bytes-evt size (current-input)))))

    (define counter (box 0))
    (define (commit byte)
      (let ((n (unbox counter)))
        (bytes-set! buffer n byte)
        (if (= (add1 n) size)
            (begin (write-bytes buffer (current-output)) (set-box! counter 0))
            (set-box! counter (add1 n)))))
    (define (flush)
      (let ((n (unbox counter)))
        (write-bytes buffer (current-output) 0 n)
        (set-box! counter 0)))

    (public set-input set-output read commit flush)))
