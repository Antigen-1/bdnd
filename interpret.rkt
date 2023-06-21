#lang racket/base
(require "codec.rkt" "huffman.rkt" racket/file "lock.rkt" racket/class "buffer.rkt")
(provide bdnd-interpret)

(define (bdnd-interpret port size filelist tree prefix)
  (file-stream-buffer-mode port 'block)

  (make-directory* prefix)
  
  (let-values (((ich thd) (decompress-from-port port (cond (size) (else 1000000))))
               ((buffer) (new out-buffer% (size (cond (size) (else 1000000))))))
    (define (check-and-get l) (if (null? l) (sync ich) l))
    (define (get) (sync ich))
    
    (define counter (box 0))
    (define (increase n) (set-box! counter (+ n (unbox counter))))

    (parameterize ((current-directory prefix))
      (foldl (lambda (f i) (let ((name (cadr f))
                                 (size (car f)))
                             (increase size)
                             (with-handlers ((exn:fail:filesystem? (lambda (e) (delete-directory/files #:must-exist? #f name) (raise e))))
                               (make-parent-directory* name)
                               (call-with-output-file/lock
                                 name
                                 (lambda (out)
                                   (file-stream-buffer-mode out 'block)
                                   (send-generic buffer set-output out)
                                   (let loop ((t tree) (l (check-and-get i)) (s size))
                                     (cond ((zero? s) (send-generic buffer flush) l)
                                           (else
                                            (define-values (ls tr) (index-huffman-tree l t))
                                            (cond ((byte? tr) (send-generic buffer commit tr) (loop tree (check-and-get ls) (sub1 s)))
                                                  (else (loop tr (get) s)))))))))))
             null
             filelist))
    (sync thd)
    (unbox counter)))
