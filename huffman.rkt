#lang racket/base
(define (init)
  (make-vector 256 0))

(define (vector-update vec pos proc)
  (vector-set! vec pos (proc (vector-ref vec pos))))

(define (|use port to update vector| port vec)
  (for ((b (in-port read-byte port)))
    (vector-update vec b add1)))

(define (path->frequency-vector path)
  (define fv (init))

  (let loop ((p path))
    (define rp (resolve-path p))
    (if (file-exists? rp)
        (call-with-input-file* rp (lambda (in) (|use port to update vector| in fv)))
        (parameterize ((current-directory rp)) (for ((pd (in-directory))) (loop pd)))))

  fv)

(define make-node list)
(define node-frequency car)
(define node-content cadr)
(define left-node caddr)
(define right-node cadddr)
(define node-is-leaf? (compose byte? node-content))

(require racket/contract)

(define node/c (or/c (list/c exact-positive-integer? set? node/c node/c)
                     (list/c exact-positive-integer? byte?)))

(define (insert-node o l (r null))
  (cond ((null? l) (reverse (cons o r)))
        ((> (node-frequency o) (node-frequency (car l))) (insert-node o (cdr l) (cons (car l) r)))
        (else (append (reverse (cons o r)) l))))

(define (sort-frequency-vector-to-list fv)
  (let loop ((i 0) (r null))
    (cond ((= i 256) r)
          ((zero? (vector-ref fv i)) (loop (add1 i) r))
          (else (loop (add1 i) (insert-node (make-node (vector-ref fv i) i) r))))))

(require set)

(define-syntax-rule (configure-byte-set expr ...)
  (parameterize ((current-key-accessor values)
                 (current-comparation-handler >)
                 (current-equation-handler =))
    expr ...))

(define (add-byte-to-set b s)
  (configure-byte-set (add-element b s)))

(define (byte-set-union s1 s2)
  (configure-byte-set (set-union s1 s2)))

(define (byte-set-have? b s)
  (configure-byte-set (have-element? b s)))

(define (merge-two-nodes n1 n2)
  (call-with-values (lambda () (if (> (node-frequency n1) (node-frequency n2)) (values n2 n1) (values n1 n2)))
                    (lambda (min max)
                      (make-node (+ (node-frequency min) (node-frequency max))
                                 (cond ((and (node-is-leaf? min) (node-is-leaf? max))
                                        (if (> (node-content min) (node-content max))
                                            (make-set (node-content max)
                                                      empty-set
                                                      (make-set (node-content min) empty-set empty-set))
                                            (make-set (node-content min)
                                                      empty-set
                                                      (make-set (node-content max) empty-set empty-set))))
                                       ((and (node-is-leaf? min) (not (node-is-leaf? max)))
                                        (add-byte-to-set (node-content min) (node-content max)))
                                       ((and (node-is-leaf? max) (not (node-is-leaf? min)))
                                        (add-byte-to-set (node-content max) (node-content min)))
                                       (else (byte-set-union (node-content min) (node-content max))))
                                 min max))))

(define/contract (ordered-list->huffman-tree l)
  (-> (non-empty-listof node/c) node/c); at least one node is required to call this function
  (cond ((null? (cdr l)) (car l))
        (else (ordered-list->huffman-tree (insert-node (merge-two-nodes (car l) (cadr l)) (cddr l))))))

(define new-node/c (list/c set? (or/c new-node/c byte?) (or/c new-node/c byte?)))

(define (cleanse-huffman-tree tree)
  (cond ((node-is-leaf? tree) (node-content tree))
        (else (list (node-content tree) (cleanse-huffman-tree (left-node tree)) (cleanse-huffman-tree (right-node tree))))))

(define (make-huffman-tree path)
  (cleanse-huffman-tree (ordered-list->huffman-tree (sort-frequency-vector-to-list (path->frequency-vector path)))))

(require sugar/cache)

(define/caching (consult-huffman-tree b t (r null))
  (cond ((and (not (byte? (cadr t))) (byte-set-have? b (car (cadr t)))) (consult-huffman-tree b (cadr t) (cons 0 r)))
        ((and (byte? (cadr t)) (= b (cadr t)))
         (reverse (cons 0 r)))
        ((and (byte? (caddr t)) (= b (caddr t)))
         (reverse (cons 1 r)))
        (else (consult-huffman-tree b (caddr t) (cons 1 r)))))

(define new-node-2/c (list/c (or/c byte? new-node-2/c) (or/c byte? new-node-2/c)))

(define (cleanse-huffman-tree-2 tree)
  (cond ((byte? tree) tree)
        (else (list (cleanse-huffman-tree-2 (cadr tree)) (cleanse-huffman-tree-2 (caddr tree))))))

(define/caching (index-huffman-tree tree list)
  (cond ((or (byte? tree) (null? list)) (cons tree list))
        (else (index-huffman-tree (if (zero? (car list)) (car tree) (cadr tree))
                                  (cdr list)))))

(provide consult-huffman-tree index-huffman-tree
         (contract-out ; check the result, in order to ensure that `consult-huffman-tree` and `index-huffman-tree` will not fail
          (make-huffman-tree (-> path-string? new-node/c))
          (cleanse-huffman-tree-2 (-> new-node/c new-node-2/c))))
