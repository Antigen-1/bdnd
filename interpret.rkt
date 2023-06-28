#lang racket/base
(require (submod "huffman.rkt" shallow) racket/file "lock.rkt" racket/class "buffer.rkt")
(provide bdnd-interpret)

(define (bdnd-interpret port size filelist tree prefix)
  (file-stream-buffer-mode port 'block)

  (make-directory* prefix)

  (define buffer-size (cond (size) (else 1000000)))
  
  (define (make-buffer %) (new % (size buffer-size)))
  
  (let ((in-buffer (make-buffer in-buffer%))
        (out-buffer (make-buffer out-buffer%)))
    (send-generic in-buffer set-input port)
    
    (define (get) (cons 8 (send-generic in-buffer read)))
    (define (check-and-get p) (if (zero? (car p)) (get) p))
    
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
                                   (send-generic out-buffer set-output out)
                                   (let loop ((t tree) (l i) (s size))
                                     (cond ((zero? s) (send-generic out-buffer flush) l)
                                           (else
                                            (define-values (bt ln tr) (index-huffman-tree (cdr l) (car l) t))
                                            (cond ((byte? tr) (send-generic out-buffer commit tr) (loop tree (check-and-get (cons ln bt)) (sub1 s)))
                                                  (else (loop tr (get) s)))))))))))
             (get)
             filelist))
    (unbox counter)))
