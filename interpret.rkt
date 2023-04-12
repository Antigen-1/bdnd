#lang racket/base
(require "codec.rkt" "huffman.rkt" racket/file)
(provide bdnd-interpret)

(define (bdnd-interpret filelist tree prefix port)
  (let-values (((ich thd) (decompress-from-port port)))
    (make-directory* prefix)
    (parameterize ((current-directory prefix))
      (foldl (lambda (f i) (let ((name (cadr f))
                                 (size (car f)))
                             (collect-garbage 'incremental)
                             (with-handlers ((exn:fail:filesystem? (lambda (e) (delete-directory/files #:must-exist? #f name) (raise e))))
                               (make-parent-directory* name)
                               (call-with-output-file*
                                 name
                                 (lambda (out) 
                                   (let loop ((t tree) (l i) (s size))
                                     (define (index l)
                                       (let ((r (index-huffman-tree t l)))
                                         (if (byte? (car r))
                                             (begin (write-byte (car r) out) (loop tree (cdr r) (sub1 s)))
                                             (loop (car r) null s))))
                                     (cond ((zero? s) l)
                                           ((null? l)
                                            (sync (handle-evt ich index)))
                                           (else (index l)))))))))
             null
             filelist))
    (sync (handle-evt thd void))))