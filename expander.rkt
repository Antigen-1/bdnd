#lang racket/base
(require "codec.rkt" "huffman.rkt" racket/file racket/async-channel racket/port (for-syntax racket/base))
(provide (rename-out (#%bdnd-module-begin #%module-begin)) #%datum)

(define-syntax-rule (#%bdnd-module-begin tree filelist prefix bytes ...)
  (#%module-begin
  (let-values (((ich thd) (decompress-from-port (input-port-append (open-input-bytes bytes) ...)))
               ((och mach) (let ((ch (make-async-channel)))
                             (values
                              ch
                              (thread (lambda ()
                                        (let loop ((t tree))
                                          (sync
                                           (handle-evt
                                            (thread-receive-evt)
                                            (lambda (_) (let-values (((v r) (index-huffman-tree t (thread-receive))))
                                                          (if (byte? v)
                                                              (begin (aysnc-channel-put ch v) (thread-rewind-receive (list r)) (loop tree))
                                                              (loop v)))))))))))))
    (make-directory* prefix)
    (parameterize ((current-directory prefix))
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
                           (loop (cdr l))
                           (sync (handle-evt ich
                                             (lambda (v)
                                               (thread-send mach v)
                                               (work s)))
                                 (handle-evt och
                                             (lambda (v)
                                               (write-byte v out)
                                               (work (sub1 s))))))))))))))))
                               
  )
