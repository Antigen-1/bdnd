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
    (check-equal? (consult-huffman-tree 97 tree) '(0))
    (check-equal? (consult-huffman-tree 98 tree) '(1 0 1))
    (check-equal? (consult-huffman-tree 99 tree) '(1 1))
    (check-equal? (consult-huffman-tree 100 tree) '(1 0 0 1))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  )
