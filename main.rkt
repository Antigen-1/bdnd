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
  (require racket/fasl racket/port)
  
  (define (read-syntax src port)
    (read-line port)
    (datum->syntax
     #f
     (append (list 'module (gensym 'bdnd) 'bdnd/expander)
             (for/list ((s (in-port (lambda (p) (if (sync/timeout 0 (eof-evt p)) eof (fasl->s-exp p))) port))) (list 'quote s)))))

  (provide read-syntax))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (require racket/async-channel racket/port "codec.rkt")

  (test-case
      "codec"
    (define-values (in out) (make-pipe))
    (define-values (ch ch1 _1) (compress-to-port))
    (define-values (ch2 ch3 _2) (decompress-from-port))
    (define bit-list '(0 1 1 0 1 0 1 1))
    (async-channel-put ch out)
    (async-channel-put ch2 in)
    (async-channel-put ch bit-list)
    (async-channel-put ch (open-output-nowhere))
    (async-channel-put ch #f)
    (check-eq? (sync (handle-evt ch1 (lambda (p) (close-output-port p) p))) out)
    (check-equal? (sync ch3) bit-list))

  (require "huffman.rkt")
  
  (test-case
      "huffman"
    (define test-file (build-path test-dir "huffman"))
    (define tree (make-huffman-tree test-file))
    (check-equal? (consult-huffman-tree 97 tree) '(0))
    (check-equal? (consult-huffman-tree 98 tree) '(1 0 1))
    (check-equal? (index-huffman-tree tree (consult-huffman-tree 99 tree)) '(99))
    (check-equal? (index-huffman-tree tree (consult-huffman-tree 100 tree)) '(100))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  
  (require racket/cmdline raco/command-name "huffman.rkt" "codec.rkt" racket/async-channel racket/port racket/fasl)
  
  (define current-prefix (make-parameter "file"))
  (define current-output-file (make-parameter "result.rkt"))
  (define current-handling-directory (make-parameter #f))

  (command-line #:program (short-program+command-name)
                #:once-any (("-d" "--directory") d "specify a directory" (current-handling-directory d))
                #:once-any (("-p" "--prefix") p "specify the prefix[default to \"file\"]" (current-prefix p))
                #:once-any (("-o" "--output") o "specify the output file[default to \"result.rkt\"]" (current-output-file o)))
  
  (define ht (make-huffman-tree (current-handling-directory)))

  (with-handlers ((exn:fail:filesystem? (lambda (e) (delete-file (current-output-file)) (raise e))))
    (call-with-output-file*
      (current-output-file)
      (lambda (out)
        (displayln "#lang bdnd" out)
        (s-exp->fasl ht out)
        (s-exp->fasl (current-prefix) out)
        (flush-output out)
        (define-values (och ich _) (compress-to-port))
        (parameterize ((current-directory (current-handling-directory)))
          (for ((f (in-directory)))
            (define-values (in-end out-end) (make-pipe))
            (async-channel-put och out-end)
            (s-exp->fasl
             (append
              (call-with-input-file* f (lambda (in)
                                         (list
                                          (for/fold ((s 0)) ((b (in-port read-byte in)))
                                            (async-channel-put och (consult-huffman-tree b ht))
                                            (add1 s))
                                          (path->string f))))
              (begin
                (async-channel-put och (open-output-nowhere))
                (let loop ((r null)) (sync (handle-evt (read-bytes-evt 1000 in-end) (lambda (b) (if (eof-object? b) (reverse r) (loop (cons b r)))))
                                           (handle-evt ich (lambda (p) (close-output-port p)))))))
             out)
            (flush-output out))
          (async-channel-put och #f))))))
