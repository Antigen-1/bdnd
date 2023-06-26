#lang racket/base
(require racket/async-channel "buffer.rkt" racket/class)

(define (compress-to-port port size)
  (define in-channel (make-async-channel size))
  (define buffer (new out-buffer% (size size)))

  (send-generic buffer set-output port)
  
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
                         (define int-len (car l))
                         (define integer (cdr l))
                         (let work ((int (bitwise-ior (arithmetic-shift integer len) rest)) (ln (+ len int-len)))
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
  
  (define thd
    (thread
     (lambda ()
       (let loop ()
         (send-generic buffer read
                       (lambda (n b)
                         (cond ((not (eof-object? n))
                                (for ((bt (in-bytes b 0 n)))
                                  (async-channel-put out-channel bt))
                                (loop)))))))))

  (values out-channel thd))

(provide decompress-from-port compress-to-port)
