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
        (call-with-input-file rp (lambda (in) (|use port to update vector| in fv)))
        (parameterize ((current-directory rp)) (for ((pd (in-directory))) (loop pd)))))

  fv)

(define make-node list)
(define node-frequency car)
(define node-content cadr)
(define left-node caddr)
(define right-node cadddr)
(define node-is-leaf? (compose byte? node-content))

(define (insert-node o l (r null))
  (cond ((null? l) (reverse (cons o r)))
        ((> (node-frequency o) (node-frequency (car l))) (insert-node o (cdr l) (cons (car l) r)))
        (else (append (reverse (cons o r)) l))))

(define (sort-frequency-vector-to-list fv)
  (let loop ((i 0) (r null))
    (cond ((= i 256) r)
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

(define (ordered-list->huffman-tree l)
  (let loop ((l l))
    (cond ((null? l) #f)
          ((null? (cdr l)) (car l))
          (else (loop (insert-node (merge-two-nodes (car l) (cadr l)) (cddr l)))))))

(define (make-huffman-tree path)
  (ordered-list->huffman-tree (sort-frequency-vector-to-list (path->frequency-vector path))))

(define (consult-huffman-tree b t (r null))
  (cond ((and (node-is-leaf? (left-node t)) (= b (node-content (left-node t)))) (reverse (cons 0 r)))
        ((and (node-is-leaf? (right-node t)) (= b (node-content (right-node t)))) (reverse (cons 1 r)))
        ((have-element? b (node-content (left-node t))) (consult-huffman-tree b (left-node t) (cons 0 r)))
        (else (consult-huffman-tree b (right-node t) (cons 1 r)))))

(provide make-huffman-tree consult-huffman-tree)
