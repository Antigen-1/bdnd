#lang racket/base
(require racket/async-channel racket/list)

(define (compress-to-port o)
  (define in-channel (make-async-channel))

  (define (bit-list->byte l (i 1) (r 0))
    (cond ((null? l) r)
          (else (bit-list->byte (cdr l) (* 2 i) (+ r (* (car l) i))))))
  
  (define thd
   (thread
    (lambda ()
      (let loop ((rest null))
        (sync (handle-evt
               in-channel
               (lambda (l)
                 (if l
                     (let ((ls (append rest l)))
                       (if (>= (length ls) 8)
                           (let-values (((former latter) (split-at ls 8)))
                             (write-byte (bit-list->byte former) o)
                             (loop latter))
                           (loop ls)))
                     (cond ((not (null? rest))
                            (let loop ((r rest))
                              (cond ((>= (length r) 8)
                                     (let-values (((f l) (split-at r 8)))
                                       (write-byte (bit-list->byte f) o)
                                       (loop l)))
                                    (else (write-byte (bit-list->byte r) o))))))))))))))
  
  (values in-channel thd))

(define (decompress-from-port i)
  (define out-channel (make-async-channel))

  (define (byte->bit-list b)
    (let loop ((b b) (r null) (n 8))
      (cond ((zero? n) (reverse r))
            (else (loop (arithmetic-shift b -1) (cons (bitwise-bit-field b 0 1) r) (sub1 n))))))
  
  (define thd
   (thread
    (lambda ()
      (for ((b (in-port read-byte i)))
        (async-channel-put out-channel (byte->bit-list b)))
      (async-channel-put out-channel #f))))

  (values out-channel thd))

(provide decompress-from-port compress-to-port)
