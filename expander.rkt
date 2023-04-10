#lang racket/base
(require "codec.rkt" "huffman.rkt" racket/file racket/async-channel racket/port (for-syntax racket/base))
(provide (rename-out (#%bdnd-module-begin #%module-begin)) (except-out (all-from-out racket/base) #%module-begin))

(define-syntax-rule (#%bdnd-module-begin tree filelist prefix bytes ...)
  (#%module-begin
  (let*-values (((ich thd) (decompress-from-port (input-port-append (open-input-bytes bytes) ...)))
                ((och mach) (let ((ch (make-async-channel)))
                              (values
                               ch
                               (thread (lambda ()
                                         (let/cc exit
                                           (let loop ((t tree) (l null))
                                             (define (index l)
                                               (let ((r (index-huffman-tree t (cond (l) (else (exit))))))
                                                 (if (byte? (car r))
                                                     (begin (async-channel-put ch (car r)) (loop tree (cdr r)))
                                                     (loop (car r) null))))
                                             (if (null? l)
                                                 (sync (handle-evt ich index))
                                                 (index l))))))))))
    (make-directory* prefix)
    (parameterize ((current-directory prefix))
      (let/cc next
      (let loop ((l filelist))
        (cond ((not (null? l))
               (define size (caar l))
               (define name (cdar l))
               (with-handlers ((exn:fail:filesystem? (lambda (e) (delete-file name) (raise e)))) ;;the exception is re-raised
                 (make-parent-directory* name)
                 (call-with-output-file
                   name
                   (lambda (out)
                     (let work ((s size))
                       (if (zero? s)
                           (next (loop (cdr l)))
                           (sync (handle-evt och
                                             (lambda (v)
                                               (write-byte v out)
                                               (work (sub1 s)))))))))))))))
    (sync (handle-evt mach void)))))
