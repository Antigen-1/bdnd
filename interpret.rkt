#lang racket/base
(require "codec.rkt" "huffman.rkt" racket/file "lock.rkt" racket/class "buffer.rkt")
(provide bdnd-interpret)

(define (bdnd-interpret filelist tree prefix port size)
  (file-stream-buffer-mode port 'block)
  (let-values (((ich thd) (decompress-from-port port (cond (size) (else 1000000))))
               ((buffer) (new out-buffer% (size (cond (size) (else 1000000))))))
    (make-directory* prefix)
    (parameterize ((current-directory prefix))
      (foldl (lambda (f i) (let ((name (cadr f))
                                 (size (car f)))
                             (collect-garbage 'incremental)
                             (with-handlers ((exn:fail:filesystem? (lambda (e) (delete-directory/files #:must-exist? #f name) (raise e))))
                               (make-parent-directory* name)
                               (call-with-output-file/lock
                                 name
                                 (lambda (out)
                                   (file-stream-buffer-mode out 'block)
                                   (send buffer set-output out)
                                   (let loop ((t tree) (l i) (s size))
                                     (cond ((zero? s) (send buffer flush) l)
                                           ((null? l) (loop t (sync ich) s))
                                           (else
                                            (collect-garbage 'incremental)
                                            (let ((r (index-huffman-tree (car l) t)))
                                              (cond ((byte? r)
                                                     (begin (send buffer commit r) (loop tree (cdr l) (sub1 s))))
                                                    (else (loop r (cdr l) s))))))))))))
             null
             filelist))
    (sync (handle-evt thd void))))
