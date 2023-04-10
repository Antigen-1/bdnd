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
                                           (let loop ((t tree))
                                             (sync
                                              (handle-evt
                                               (choice-evt ich (thread-receive-evt))
                                               (lambda (l) (let ((r (index-huffman-tree t (cond ((evt? l) (thread-receive)) (l) (else (exit))))))
                                                             (if (byte? (car r))
                                                                 (begin (async-channel-put ch (car r)) (thread-rewind-receive (list (cdr r))) (loop tree))
                                                                 (loop (car r)))))))))))))))
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
                                               (work (sub1 s))))))))))))))))
                               
  ))
