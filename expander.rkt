#lang racket/base
(require "codec.rkt" "huffman.rkt" racket/file (for-syntax racket/base))
(provide (rename-out (#%bdnd-module-begin #%module-begin)))

(define-syntax-rule (#%bdnd-module-begin tree filelist prefix port)
  (begin
    (make-directory* prefix)
    (define-values (ch thd) (decompress-from-port port))
    (let loop ((l filelist) (r null))
      (cond ((not (null? l))
             (define size (caar l))
             (define name (cdar l))
             (define path (build-path prefix name))
             (make-parent-directory* path)
             (with-handlers ((exn:fail:filesystem? (lambda (_) (delete-file path))))
               (call-with-output-file
                 path
                 (lambda (out)
                   (let work ((s size) (t tree) (r r))
                     (define ch-evt (handle-evt ch (lambda (v) (work s t (if v (append r v) r)))))
                     (define hn-evt (handle-evt
                                     always-evt
                                     (lambda (_) (let* ((next-instruction (car r))
                                                        (next-tree (if (zero? next-instruction) (left-node t) (right-node t))))
                                                   (if (node-is-leaf? next-tree)
                                                       (begin (write-byte (node-content next-tree) out) (if (= 1 s) (loop (cdr l) (cdr r)) (work (sub1 s) tree (cdr r))))
                                                       (work s next-tree (cdr r)))))))
                     (sync ch-evt
                           (replace-evt thd (lambda (_) (cond ((sync/timeout 0 ch-evt)) ((not (null? r)) hn-evt)))) ;;exit
                           (if (null? r) never-evt hn-evt)))))))))))
                               
