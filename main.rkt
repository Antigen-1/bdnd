#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require racket/runtime-path)

(define-runtime-path test-dir "test")

(module reader racket/base
  (define (read-syntax src port)
    (datum->syntax
     #f
     (append (list 'module (gensym 'bdnd) 'bdnd/expander)
             (let loop ((r null))
               (define v (read port))
               (cond ((eof-object? v) (reverse r))
                     (else (loop (cons (list 'quote v) r))))))))

  (provide read-syntax))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (require racket/async-channel "codec.rkt")

  (test-case
      "codec"
    (define-values (in out) (make-pipe))
    (define-values (ch1 thd1) (compress-to-port out))
    (define-values (ch2 _) (decompress-from-port in))
    (define bit-list '(0 1 1 0 1 0 1 1))
    (async-channel-put ch1 bit-list)
    (async-channel-put ch1 #f)
    (sync (handle-evt (thread-dead-evt thd1) (lambda (_) (close-output-port out))))
    (check-equal? bit-list (sync ch2))
    (check-eq? #f (sync ch2)))

  (require "huffman.rkt")
  
  (test-case
      "huffman"
    (define test-file (build-path test-dir "huffman"))
    (define tree (make-huffman-tree test-file))
    (define-values (byte rest) (index-huffman-tree tree '(1 0 0 1)))
    (check-eq? rest null)
    (check-eq? byte 100)
    (check-equal? (consult-huffman-tree 97 tree) '(0))
    (check-equal? (consult-huffman-tree 98 tree) '(1 0 1))
    (check-equal? (consult-huffman-tree 99 tree) '(1 1))
    (check-equal? (consult-huffman-tree 100 tree) '(1 0 0 1))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  
  (require racket/cmdline raco/command-name "huffman.rkt" "codec.rkt" racket/async-channel racket/port)
  
  (define current-prefix (make-parameter "file"))
  (define current-output-file (make-parameter "result.rkt"))
  (define current-handling-directory (make-parameter #f))

  (command-line #:program (short-program+command-name)
                #:once-any (("-d" "--directory") d "specify a directory" (current-handling-directory d))
                #:once-any (("-p" "--prefix") p "specify the prefix[default to \"file\"]" (current-prefix p))
                #:once-any (("-o" "--output") o "specify the output file[default to \"result.rkt\"]" (current-output-file o)))
  
  (define ht (make-huffman-tree (current-handling-directory)))

  (define fl (parameterize ((current-directory (current-handling-directory)))
               (reverse
                (for/fold ((r null)) ((p (in-directory)))
                  (if (file-exists? p) (cons (cons (file-size p) (path->string p)) r) r))))) ;; The predicate file-exists? works on the final destination of a link or series of links.
  
  (call-with-output-file
    (current-output-file)
    (lambda (out)
      (displayln "#lang bdnd" out)
      (write ht out)
      (write fl out)
      (write (current-prefix) out)
      (define-values (in-end out-end) (make-pipe))
      (define-values (ch compress-thd) (compress-to-port out-end))
      (define writer-thd (thread (lambda () (let loop () (sync (handle-evt compress-thd (lambda (_) (close-output-port out-end) (loop)))
                                                               (handle-evt (read-bytes-evt 1000 in-end) (lambda (b) (cond ((not (eof-object? b)) (write b out) (loop))))))))))
      (parameterize ((current-directory (current-handling-directory)))
        (map (lambda (f) (call-with-input-file (cdr f) (lambda (in) (for ((b (in-port read-byte in))) (async-channel-put ch (consult-huffman-tree b ht)))))) fl))
      (thread-wait writer-thd))))
