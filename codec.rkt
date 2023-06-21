#lang racket/base
(require racket/async-channel racket/list "buffer.rkt" racket/class)

(define (compress-to-port port size)
  (define in-channel (make-async-channel size))
  (define buffer (new out-buffer% (size size)))

  (send-generic buffer set-output port)
  
  (define (bit-list->byte l (i 1) (r 0))
    (cond ((null? l) r)
          ((zero? (car l)) (bit-list->byte (cdr l) (* 2 i) r))
          (else (bit-list->byte (cdr l) (* 2 i) (+ r i)))))
  
  (define thd
    (thread
     (lambda ()
       (let loop ((rest null) (len 0))
         (sync (handle-evt
                in-channel
                (lambda (l)
                  (cond ((not l) (cond ((not (zero? len)) (send-generic buffer commit (bit-list->byte rest))))
                                 (send-generic buffer flush))
                        (else
                         (let work ((ls (append rest l)) (ln (+ len (length l))))
                           (if (>= ln 8)
                               (let-values (((former latter) (split-at ls 8)))
                                 (send-generic buffer commit (bit-list->byte former))
                                 (work latter (- ln 8)))
                               (loop ls ln))))))))))))
                     
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
         (collect-garbage 'incremental) ;;call `collect-garbage` with a periodic task
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
