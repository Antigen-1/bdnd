#lang racket/base
(require "tree.rkt" "buffer.rkt" "lock.rkt" "huffman.rkt"
         racket/match racket/class
         (for-syntax racket/base racket/list))
(provide make-compress-handler make-decompress-handler)

;;very simple macro used to make local states
(define-syntax (define-states stx)
  (syntax-case stx ()
    ((_ (state init) ...)
     (let ((len (length (syntax->list #'(state ...)))))
       (with-syntax (((ind ...) (range 0 len)))
         #'(define-values (state ...)
             (let ((states (vector init ...)))
               (values
                (case-lambda (() (vector-ref states ind))
                             ((v) (vector-set! states ind v)))
                ...))))))))

;;computational objects
(define (make-compress-handler table output size)
  (define-states (remain 0) (remain-length 0))
  (define in-buffer (new in-buffer% (size size)))
  (define out-buffer (new out-buffer% (size size)))
  (send-generic out-buffer set-output output)
  (lambda (fl)
    (match fl
      ((file name _)
       (call-with-input-file/lock
        name
        (lambda (in)
          (send-generic in-buffer set-input in)
          (let loop ((s 0) (r (remain)) (rl (remain-length)))
            (define byte (send-generic in-buffer read))
            (cond ((eof-object? byte) (remain r) (remain-length rl) (set-file-size! fl s))
                  (else
                   (define pair (hash-ref table byte))
                   (let work ((r (bitwise-ior r (arithmetic-shift (cdr pair) rl))) (rl (+ (car pair) rl)))
                     (if (>= rl 8)
                         (begin (send-generic out-buffer commit (bitwise-bit-field r 0 8)) (work (arithmetic-shift r -8) (- rl 8)))
                         (loop (add1 s) r rl)))))))))
      (#f (send-generic out-buffer commit (remain)) (send-generic out-buffer flush)))))
(define (make-decompress-handler tree input size)
  (define indexer (make-indexer tree))
  (define-states (remain 0) (remain-length 0))
  (define in-buffer (new in-buffer% (size size)))
  (define out-buffer (new out-buffer% (size size)))
  (send-generic in-buffer set-input input)
  (define (get)
    (remain (send-generic in-buffer read))
    (remain-length 8))
  (define (check-and-get) (cond ((zero? (remain-length)) (get))))
  (define (update r rl) (remain r) (remain-length rl))

  ;;the `size` field instead of an `eof` determines the real size of the resulting file
  (lambda (fl)
    (match fl
      ((file name size)
       (call-with-output-file/lock
        name
        (lambda (out)
          (send-generic out-buffer set-output out)
          (let loop ((s size))
            (cond ((zero? s) (send-generic out-buffer flush))
                  (else
                   (define-values (rm ln res) (indexer (remain) (remain-length)))
                   (cond (res (send-generic out-buffer commit res)
                              (update rm ln)
                              (check-and-get)
                              (loop (sub1 s)))
                         (else (get) (loop s))))))))))))
