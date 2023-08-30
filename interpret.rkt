#lang racket/base
(require "codec.rkt" "tree.rkt" tree racket/file)
(provide bdnd-interpret)

(define (bdnd-interpret port size tree path-tree)
  (file-stream-buffer-mode port 'block)
  
  (define buffer-size (cond (size) (else 1000000)))
  
  (define handler (make-decompress-handler tree port buffer-size))

  (with-handlers ((exn:fail:filesystem? (lambda (exn) (delete-directory/files (label path-tree) #:must-exist? #f) (raise exn))))
    (iter-path-tree handler path-tree)))
