#lang racket/base
(require racket/class racket/generator racket/port)
(provide in-buffer% out-buffer% read set-input commit flush set-output)

(define in-buffer%
  (class object%
    (super-new)

    (init-field size)
    
    (define current-input (box (current-input-port)))
    (define gen
      (let ((buffer (make-bytes size)))
        (generator
         ()
         (let loop ()
           (sync (handle-evt (read-bytes!-evt buffer (unbox current-input))
                             (lambda (n)
                               (cond ((eof-object? n) (yield eof))
                                     (else (for ((bt (in-bytes buffer 0 n)))
                                             (yield bt))))
                               (loop))))))))
    
    (define (set-input port) (set-box! current-input port))
    (define (read) (gen))

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
            (begin (write-bytes buffer (unbox current-output)) (set-box! counter 0))
            (set-box! counter (add1 n)))))
    (define (flush)
      (let ((n (unbox counter)))
        (write-bytes buffer (unbox current-output) 0 n)
        (set-box! counter 0)))

    (public set-output commit flush)))

(define read (generic in-buffer% read))
(define set-input (generic in-buffer% set-input))
(define commit (generic out-buffer% commit))
(define set-output (generic out-buffer% set-output))
(define flush (generic out-buffer% flush))
