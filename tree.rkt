#lang racket/base
(require tree racket/list racket/contract racket/match racket/file)
(provide (contract-out (struct file ((name path?) (size (or/c #f exact-nonnegative-integer?))))
                       (make-path-tree (-> path-string? any))
                       (iter-path-tree (-> (-> any/c any) tree? any))))

;;structures
(struct file (name (size #:mutable #:auto)) #:prefab #:constructor-name make-file-node)
(struct info (base depth) #:transparent)

;;path utilities
(define (path-truncate-for path num)
  (apply build-path (reverse (drop (reverse (explode-path path)) num))))
(define (last-path-element path)
  (call-with-values (lambda () (split-path path))
                    (lambda lst (cadr lst))))

;;path tree constructor
(define (make-path-tree directory-path)
  (if (directory-exists? directory-path)
      (parameterize ((current-directory directory-path))
        (apply tree (last-path-element directory-path)
               (filter-map
                (lambda (p) (cond ((directory-exists? p) (make-path-tree p))
                                  ((file-exists? p) (tree (make-file-node p)))
                                  (else #f)))
                (directory-list))))
      (raise (exn:fail:filesystem (format "Directory ~a is not found." directory-path) (current-continuation-marks)))))

;;path tree iterator
(define (iter-path-tree proc tree)
  (void
   (fold/with-depth
    (lambda (node inf depth)
      (match* (node inf)
        (((file name _) (info base dep))
         (cond ((> dep depth)
                (define new-base (path-truncate-for base (- dep depth)))
                (parameterize ((current-directory new-base))
                  (proc node))
                (info new-base depth))
               (else (parameterize ((current-directory base))
                       (proc node))
                     inf)))
        ((dir (info base dep))
         (cond ((> dep depth)
                (define parent (path-truncate-for base (- dep depth)))
                (parameterize ((current-directory parent))
                  (make-directory* dir))
                (info (build-path parent dir) (add1 depth)))
               (else (parameterize ((current-directory base))
                       (make-directory* dir))
                     (info (build-path base dir) (add1 dep)))))))
    (info "." 0)
    tree)))