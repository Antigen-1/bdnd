#lang racket/base
(define (init)
  (make-vector 256 0))

(define (vector-update vec pos proc)
  (vector-set! vec pos (proc (vector-ref vec pos))))

(define (|use port to update vector| port vec)
  (for ((b (in-port read-byte port)))
    (vector-update vec b add1)))

(require "lock.rkt")

(define (path->frequency-vector path)
  (define fv (init))

  (let loop ((p path))
    (define rp (resolve-path p))
    (if (file-exists? rp)
        (call-with-input-file/lock rp (lambda (in) (|use port to update vector| in fv)))
        (parameterize ((current-directory rp)) (for ((pd (in-directory))) (loop pd)))))

  fv)

(define make-node list)
(define node-frequency car)
(define node-content cadr)
(define left-node caddr)
(define right-node cadddr)
(define node-is-leaf? (compose byte? node-content))

(require racket/contract)

(define leaf/c (list/c any/c byte?))
(define node/c (list/c any/c not any/c any/c))

(define (insert-node o l (r null))
  (cond ((null? l) (reverse (cons o r)))
        ((> (node-frequency o) (node-frequency (car l))) (insert-node o (cdr l) (cons (car l) r)))
        (else (append (reverse (cons o r)) l))))

(define (sort-frequency-vector-to-list fv)
  (let loop ((i 0) (r null))
    (cond ((= i 256) r)
          ((zero? (vector-ref fv i)) (loop (add1 i) r))
          (else (loop (add1 i) (insert-node (make-node (vector-ref fv i) i) r))))))

(define (merge-two-nodes n1 n2)
  (call-with-values (lambda () (if (> (node-frequency n1) (node-frequency n2)) (values n2 n1) (values n1 n2)))
                    (lambda (min max)
                      (make-node (+ (node-frequency min) (node-frequency max))
                                 #f ;;generating the set is not necessary because in fact I'll never consult it
                                 min max))))

(define/contract (ordered-list->huffman-tree l)
  (-> (non-empty-listof (or/c node/c leaf/c)) node/c); at least one element is required to call this function and the value returned by this function must not be a leaf
  (cond ((null? (cdr l)) (car l))
        (else (ordered-list->huffman-tree (insert-node (merge-two-nodes (car l) (cadr l)) (cddr l))))))

(define (make-huffman-tree path)
  (ordered-list->huffman-tree (sort-frequency-vector-to-list (path->frequency-vector path))))

(require racket/format)

(define (analyze-compression-ratio tree)
  (define vec (vector 0 0))
  (let loop ((depth 0) (tree tree))
    (cond ((node-is-leaf? tree) (vector-set! vec 0 (+ (node-frequency tree) (vector-ref vec 0)))
                                (vector-set! vec 1 (+ (/ (* (node-frequency tree) depth) 8)
                                                      (vector-ref vec 1))))
          (else (loop (add1 depth) (left-node tree))
                (loop (add1 depth) (right-node tree)))))
  (~a
   (~r #:precision '(= 1)
       (* 100 (/ (vector-ref vec 1) (vector-ref vec 0))))
   "%"))

(define (huffman-tree->hash-table t)
  (define h (make-hasheq))
  (let loop ((t t) (r null))
    (cond ((node-is-leaf? t) (hash-set! h (node-content t) (reverse r)))
          (else (loop (left-node t) (cons 0 r))
                (loop (right-node t) (cons 1 r)))))
  h)

(define (consult-huffman-tree b t)
  (hash-ref t b))

(define (cleanse-huffman-tree tree)
  (cond ((node-is-leaf? tree) (node-content tree))
        (else (list (cleanse-huffman-tree (left-node tree)) (cleanse-huffman-tree (right-node tree))))))

(define (index-huffman-tree ins tree)
  (if (zero? ins) (car tree) (cadr tree)))

(provide consult-huffman-tree index-huffman-tree make-huffman-tree cleanse-huffman-tree huffman-tree->hash-table
         analyze-compression-ratio)
