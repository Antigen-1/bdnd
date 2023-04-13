#lang racket/base
(require racket/async-channel racket/list sugar/cache)

(define (compress-to-port port)
  (define in-channel (make-async-channel))

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
                  (cond ((not l) (cond ((not (zero? len)) (write-byte (bit-list->byte rest) port))))
                        (else
                         (let work ((ls (append rest l)) (ln (+ len (length l))))
                           (if (>= ln 8)
                               (let-values (((former latter) (split-at ls 8)))
                                 (write-byte (bit-list->byte former) port)
                                 (work latter (- ln 8)))
                               (loop ls ln))))))))))))
                     
  (values in-channel thd))

(define (decompress-from-port port)
  (define out-channel (make-async-channel))

  (define/caching (byte->bit-list b)
    (let loop ((b b) (r null) (n 8))
      (cond ((zero? n) (reverse r))
            (else (loop (arithmetic-shift b -1) (cons (bitwise-bit-field b 0 1) r) (sub1 n))))))
  
  (define thd
    (thread
     (lambda ()
       (for ((b (in-port read-byte port)))
         (async-channel-put out-channel (byte->bit-list b))))))

  (values out-channel thd))

(provide decompress-from-port compress-to-port)
