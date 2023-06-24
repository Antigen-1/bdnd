#lang racket/base
(require racket/async-channel racket/list "buffer.rkt" racket/class)

(define (compress-to-port port size)
  (define in-channel (make-async-channel size))
  (define buffer (new out-buffer% (size size)))

  (send-generic buffer set-output port)
  
  (define (bit-list->int l (i 1) (r 0))
    (cond ((null? l) r)
          ((zero? (car l)) (bit-list->int (cdr l) (arithmetic-shift i 1) r))
          (else (bit-list->int (cdr l) (arithmetic-shift i 1) (bitwise-ior r i)))))
  
  (define thd
    (thread
     (lambda ()
       (let loop ((rest 0) (len 0))
         (sync (handle-evt
                in-channel
                (lambda (l)
                  (cond ((not l) (cond ((not (zero? len)) (send-generic buffer commit rest)))
                                 (send-generic buffer flush))
                        (else
                         (let work ((int (bit-list->int l (arithmetic-shift 1 len) rest)) (ln (+ len (length l))))
                           (if (>= ln 8)
                               (begin
                                 (send-generic buffer commit (bitwise-bit-field int 0 8))
                                 (work (arithmetic-shift int -8) (- ln 8)))
                               (loop int ln))))))))))))
                     
  (values in-channel thd))

(define (decompress-from-port port size)
  (define out-channel (make-async-channel size))
  (define buffer (new in-buffer% (size size)))

  (send-generic buffer set-input port)
  
  (define (byte->bit-list b (r null) (n 8))
    (cond ((zero? n) (reverse r))
          (else (byte->bit-list (arithmetic-shift b -1) (cons (bitwise-bit-field b 0 1) r) (sub1 n)))))
  
  (define thd
    (thread
     (lambda ()
       (let loop ()
         (send-generic buffer read
                       (lambda (n b)
                         (cond ((not (eof-object? n))
                                (let work ((i 0))
                                  (cond ((= i n) (loop))
                                        (else
                                         (async-channel-put out-channel
                                                            (byte->bit-list (bytes-ref b i)))
                                         (work (add1 i)))))))))))))

  (values out-channel thd))

(provide decompress-from-port compress-to-port)
