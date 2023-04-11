#lang racket/base
(require racket/async-channel racket/list sugar/cache)

(define (compress-to-port)
  (define in-channel (make-async-channel))
  (define flag-channel (make-async-channel))

  (define/caching (bit-list->byte l (i 1) (r 0))
    (cond ((null? l) r)
          (else (bit-list->byte (cdr l) (* 2 i) (+ r (* (car l) i))))))
  
  (define thd
    (thread
     (lambda ()
       (let loop ((rest null) (port #f))
         (sync (handle-evt
                in-channel
                (lambda (l)
                  (if (output-port? l)
                      (begin (cond ((null? rest)) (else (write-byte (bit-list->byte rest) port)))
                             (async-channel-put flag-channel #f)
                             (loop null l))
                      (let work ((ls (if l (append rest l) rest)))
                        (if (>= (length ls) 8)
                            (let-values (((former latter) (split-at ls 8)))
                              (write-byte (bit-list->byte former) port)
                              (work latter))
                            (cond (l (loop ls port))
                                  ((null? ls))
                                  (else (write-byte (bit-list->byte ls) port)))))))))))))
                     
  (values in-channel flag-channel thd))

(define (decompress-from-port)
  (define in-channel (make-async-channel))
  (define out-channel (make-async-channel))

  (define/caching (byte->bit-list b)
    (let loop ((b b) (r null) (n 8))
      (cond ((zero? n) (reverse r))
            (else (loop (arithmetic-shift b -1) (cons (bitwise-bit-field b 0 1) r) (sub1 n))))))
  
  (define thd
    (thread
     (lambda ()
       (let loop ()
         (sync in-channel
               (lambda (port)
                 (if port
                     (begin
                       (for ((b (in-port read-byte port)))
                         (async-channel-put out-channel (byte->bit-list b)))
                       (loop))
                     (void))))))))

  (values in-channel out-channel thd))

(provide decompress-from-port compress-to-port)
