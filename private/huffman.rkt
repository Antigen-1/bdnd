#lang typed/racket/base/deep
(require/typed "tree.rkt"
  (#:opaque File file?)
  (#:opaque Path-Tree path-tree?)
  (file-name (-> File Path-String))
  (iter-path-tree (-> (-> File Any) Path-Tree Any)))
(require/typed "lock.rkt" (call-with-input-file/lock (-> Path-String (-> Input-Port Any) Any)))
;;All aliases
(define-type Frequency-Vector (Vectorof Natural))
(define-type Path-String (U Path String))
(define-type Leaf (List Exact-Positive-Integer Byte))
(define-type Node (Rec N (List Exact-Positive-Integer False (U N Leaf) (U N Leaf))))
(define-type Node-or-Leaf (U Leaf Node))
(define-type Instructions Natural)
(define-type Cleansed (Rec N (List (U Byte N) (U Byte N))))
(define-type Table (Immutable-HashTable Byte (Pair Exact-Positive-Integer Instructions)))

;;Typed Functions
(define (init) : Frequency-Vector
  (make-vector 256 0))

(define (vector-update (vec : Frequency-Vector) (pos : Natural) (proc : (-> Natural Natural)))
  (vector-set! vec pos (proc (vector-ref vec pos))))

(define (|use port to update vector| (port : Input-Port) (vec : Frequency-Vector))
  (for ((b (in-port read-byte port)))
    (vector-update vec b add1)))

(define (path->frequency-vector (parent : Path-String) (path-tree : Path-Tree)) : Frequency-Vector
  (define fv (init))

  (parameterize ((current-directory parent))
    (iter-path-tree
     (lambda (node)
       (call-with-input-file/lock (file-name node) (lambda (in) (|use port to update vector| in fv))))
     path-tree))

  fv)

;;;Node and leaf operations
;;;------------------------------------------------------------------------------------
(: make-node : (case-> (Exact-Positive-Integer Byte -> Leaf)
                       (Exact-Positive-Integer False Node-or-Leaf Node-or-Leaf -> Node)))
(: node-frequency (-> Node-or-Leaf Exact-Positive-Integer))
(: leaf-content (-> Leaf Byte))
(: left-node (-> Node Node-or-Leaf))
(: right-node (-> Node Node-or-Leaf))
(define make-node
  (case-lambda (((f : Exact-Positive-Integer) (c : Byte)) (list f c))
               (((f : Exact-Positive-Integer) (c : False) (l : Node-or-Leaf) (r : Node-or-Leaf)) (list f c l r))))
(define node-frequency car)
(define leaf-content cadr)
(define left-node caddr)
(define right-node cadddr)
(define node-is-leaf? (make-predicate Leaf))

(: insert-node (->* (Node-or-Leaf (Listof Node-or-Leaf)) ((Listof Node-or-Leaf)) (Listof Node-or-Leaf)))
(define (insert-node o l (r null))
  (cond ((null? l) (reverse (cons o r)))
        ((> (node-frequency o) (node-frequency (car l))) (insert-node o (cdr l) (cons (car l) r)))
        (else (append (reverse (cons o r)) l))))
;;;------------------------------------------------------------------------------------

(: sort-frequency-vector-to-list (->* (Frequency-Vector) (Natural (Listof Node-or-Leaf)) (Listof Node-or-Leaf)))
(define (sort-frequency-vector-to-list fv (i 0) (r null))
  (cond ((byte? i)
         (define v (vector-ref fv i))
         (sort-frequency-vector-to-list
          fv (add1 i)
          (if (zero? v) r (insert-node (make-node v i) r))))
        (else r)))

(define (merge-two-nodes (n1 : Node-or-Leaf) (n2 : Node-or-Leaf))
  (call-with-values (lambda () (if (> (node-frequency n1) (node-frequency n2)) (values n2 n1) (values n1 n2)))
                    (lambda ((min : Node-or-Leaf) (max : Node-or-Leaf))
                      (make-node (+ (node-frequency min) (node-frequency max))
                                 #f ;;generating the set is not necessary because in fact I'll never consult it
                                 min max))))

(: ordered-list->huffman-tree (-> (Listof Node-or-Leaf) Node))
(define (ordered-list->huffman-tree l)
  (cond ((null? l) (raise (make-exn:fail:contract "There is no byte" (current-continuation-marks))))
        ((null? (cdr l)) (define v (car l)) (if ((make-predicate Node) v) v (raise (make-exn:fail:contract "There is only one single byte" (current-continuation-marks)))))
        (else (ordered-list->huffman-tree (insert-node (merge-two-nodes (car l) (cadr l)) (cddr l))))))

(define (make-huffman-tree (parent : Path-String) (path-tree : Path-Tree))
  (ordered-list->huffman-tree (sort-frequency-vector-to-list (path->frequency-vector parent path-tree))))

(require racket/format)

(define (analyze-compression-ratio (tree : Node))
  (define vec : (Vector Natural Exact-Rational) (vector 0 0))
  (let loop ((depth 0) (tree : Node-or-Leaf tree))
    (cond ((node-is-leaf? tree)
           (define f (node-frequency tree))
           (vector-set! vec 0 (+ f (vector-ref vec 0)))
           (vector-set! vec 1 (+ (/ (* f depth) 8)
                                 (vector-ref vec 1))))
          (else (loop (add1 depth) (left-node tree))
                (loop (add1 depth) (right-node tree)))))
  (~a
   (~r #:precision '(= 1)
       (* 100 (/ (ceiling (vector-ref vec 1)) (vector-ref vec 0))))
   "%"))

(define (huffman-tree->hash-table (t : Node)) : Table
  (define (handle (v : Instructions) (d : Natural) (s : (U One Zero))) : Natural
    (bitwise-ior v (arithmetic-shift s d)))
  
  (let loop ((t : Node t) (r : Instructions 0) (d : Natural 0) (h ((inst hasheq Byte (Pair Exact-Positive-Integer Instructions)))))
    (define left (left-node t))
    (define right (right-node t))
    (cond ((and (node-is-leaf? left) (node-is-leaf? right))
           (hash-set*
            h
            (leaf-content left) (cons (add1 d) (handle r d 0))
            (leaf-content right) (cons (add1 d) (handle r d 1))))
          ((node-is-leaf? left) (loop right (handle r d 1) (add1 d) (hash-set h (leaf-content left) (cons (add1 d) (handle r d 0)))))
          ((node-is-leaf? right) (loop left (handle r d 0) (add1 d) (hash-set h (leaf-content right) (cons (add1 d) (handle r d 1)))))
          (else (loop right (handle r d 1) (add1 d) (loop left (handle r d 0) (add1 d) h))))))

(: cleanse-huffman-tree (-> Node-or-Leaf (U Cleansed Byte)))
(define (cleanse-huffman-tree tree)
  (cond ((node-is-leaf? tree) (leaf-content tree))
        (else (list (cleanse-huffman-tree (left-node tree)) (cleanse-huffman-tree (right-node tree))))))

(: make-indexer (-> Cleansed (-> Instructions Natural (Values Instructions Natural (U False Byte)))))
(define (make-indexer tree)
  (let ((cursor : (Boxof (U Cleansed Byte)) (box tree)))
    (define (func (ins : Instructions) (len : Natural)) : (Values Instructions Natural (U False Byte))
      (let ((remain ((inst unbox (U Cleansed Byte)) cursor)))
        (cond ((byte? remain) (set-box! cursor tree) (values ins len remain))
              ((zero? len) (values 0 0 #f))
              (else
               (set-box! cursor (if (zero? (bitwise-bit-field ins 0 1)) (car remain) (cadr remain)))
               (func (arithmetic-shift ins -1) (sub1 len))))))
    func))

(provide make-huffman-tree cleanse-huffman-tree huffman-tree->hash-table
         analyze-compression-ratio make-indexer)
