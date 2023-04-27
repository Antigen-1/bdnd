#lang racket/base
(require racket/class racket/port)
(provide (all-defined-out))

(define in-buffer%
  (class object%
    (super-new)

    (init-field size)

    (define buffer (make-bytes size))
    
    (define current-input (box (current-input-port)))

    (define (set-input port) (set-box! current-input port))

    (define (read (handler #f))
      (sync (if handler (handle-evt (read-bytes!-evt buffer (current-input)) (lambda (n) (handler n buffer))) (read-bytes-evt size (current-input)))))

    (public set-input read)))

(define out-buffer%
  (class object%
    (super-new)

    (init-field size)

    (define buffer (make-bytes size))

    (define current-output (box (current-output-port)))

    (define (set-output port) (set-box! current-output port))
    
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

    (public set-output commit flush)))

(define read (generic in-buffer% read))
(define set-input (generic in-buffer% set-input))
(define commit (generic out-buffer% commit))
(define set-output (generic out-buffer% set-output))
(define flush (generic out-buffer% flush))
