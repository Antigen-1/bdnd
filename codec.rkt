#lang racket/base
(require racket/async-channel racket/list sugar/cache "buffer.rkt" racket/class)

(define (compress-to-port port size)
  (define in-channel (make-async-channel size))
  (define buffer (new buffer% (size (integer-sqrt size))))

  (send buffer set-output port)
  
  (define/caching (bit-list->byte l (i 1) (r 0))
    (cond ((null? l) r)
          (else (bit-list->byte (cdr l) (* 2 i) (+ r (* (car l) i))))))
  
  (define thd
    (thread
     (lambda ()
       (let loop ((rest null) (len 0))
         (sync (handle-evt
                in-channel
                (lambda (l)
                  (cond ((not l) (cond ((not (zero? len)) (send buffer commit (bit-list->byte rest))))
                                 (send buffer flush))
                        (else
                         (let work ((ls (append rest l)) (ln (+ len (length l))))
                           (if (>= ln 8)
                               (let-values (((former latter) (split-at ls 8)))
                                 (send buffer commit (bit-list->byte former))
                                 (work latter (- ln 8)))
                               (loop ls ln))))))))))))
                     
  (values in-channel thd))

(define (decompress-from-port port size)
  (define out-channel (make-async-channel size))
  (define buffer (new buffer% (size (integer-sqrt size))))

  (send buffer set-input port)
  
  (define/caching (byte->bit-list b (r null) (n 8))
    (cond ((zero? n) (reverse r))
          (else
           (define-values (qt rm) (quotient/remainder b 2))
           (byte->bit-list qt (cons rm r) (sub1 n)))))
  
  (define thd
    (thread
     (lambda ()
       (let loop ()
         (send buffer read
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
