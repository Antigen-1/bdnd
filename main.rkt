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
(provide get-buffer-size get-verbose-mode)

(define-runtime-path test-dir "test")

;;environment variables
(define (get-buffer-size)
  (let ((r (getenv "BDND_BUFFER_SIZE")))
    (or (and r (string->number r)) 1000000)))
(define (get-verbose-mode)
  (getenv "BDND_VERBOSE_MODE"))

(module* reader racket/base
  (require racket/fasl tree "private/codec.rkt" "private/tree.rkt" (submod ".."))

  (define (bdnd-interpret port size tree path-tree)
    (file-stream-buffer-mode port 'block)

    (cond ((get-verbose-mode)
           (print-tree
            (map-tree
             (lambda (node) (if (file? node)
                                (format "filename : ~a, size : ~a bytes" (file-name node) (file-size node))
                                (format "directory : ~a" node)))
             path-tree))))
    
    (define handler (make-decompress-handler tree port size))
    
    (iter-path-tree handler path-tree))
  
  (define (read-syntax _ port)
    (cond ((port-try-file-lock? port 'shared)
           (dynamic-wind
             void
             (lambda ()
               (file-stream-buffer-mode port 'block)
               (read-line port)
               (apply bdnd-interpret port (get-buffer-size) (fasl->s-exp port)))
             (lambda () (port-file-unlock port))))
          (else (raise (make-exn:fail:filesystem
                        "fail to acquire the file lock when reading the source file"
                        (current-continuation-marks))))))
  
  (provide read-syntax))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (require "private/buffer.rkt" racket/random racket/class)

  (test-case
      "buffer"
    (define in-buffer (new in-buffer% (size 1000)))
    (define out-buffer (new out-buffer% (size 1000)))
    (define-values (in out) (make-pipe))
    (send-generic in-buffer set-input in)
    (send-generic out-buffer set-output out)
    (define bts (crypto-random-bytes 1000000))
    (for ((b (in-bytes bts)))
      (send-generic out-buffer commit b))
    (send-generic out-buffer flush)
    (close-output-port out)
    (let work ((b #""))
      (define bt (send-generic in-buffer read))
      (cond ((eof-object? bt) (check-equal? bts b))
            (else (work (bytes-append b (bytes bt)))))))

  (require "private/tree.rkt" "private/huffman.rkt" "private/huffman.rkt")
  
  (test-case
      "huffman"
    (define path-tree (make-path-tree test-dir))
    (define tree (make-huffman-tree (path-up-for test-dir 1) path-tree))
    (define ctree (cleanse-huffman-tree tree))
    (define indexer (make-indexer ctree))
    (define table (huffman-tree->hash-table tree))
    (define (check byte) (check-eq? (caddr (call-with-values (lambda () (define p (hash-ref table byte)) (indexer (cdr p) (car p))) list)) byte))
    (map check '(97 98 99 100))))

(module* main racket/base
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  
  (require racket/cmdline raco/command-name racket/port racket/fasl racket/file racket/contract
           "private/huffman.rkt" "private/lock.rkt" "private/tree.rkt" "private/codec.rkt"
           (submod ".."))
  
  (define current-output-file (make-parameter "result.rkt"))
  (define current-handling-directory (make-parameter #f))
  (define current-working-directory (make-parameter (current-directory)))
  (define current-buffer-size (make-parameter (get-buffer-size)))
  (define current-verbose-mode (make-parameter (get-verbose-mode)))
  (define current-log-handler (make-parameter displayln))

  (command-line #:program (short-program+command-name)
                #:once-each
                (("-w" "--working") w "Specify the working directory" (current-working-directory w))
                (("-b" "--buffer") b "Specify the size of the buffer"
                                   (cond ((string->number b) => current-buffer-size)))
                (("-v" "--verbose") "Increase verbosity"
                                    (current-verbose-mode #t))
                (("-l" "--log") "Report information at the `info` level with the topic `bdnd`"
                                (current-log-handler (lambda (s) (log-message (current-logger) 'info 'bdnd s))))
                (("-d" "--directory") d "Specify a relative directory path"
                                      (contract relative-path? d '--directory 'current-handling-directory) ;;`iter-path-tree` function fails to handle absolute paths.
                                      (current-handling-directory d))
                (("-o" "--output") o "Specify the output file[default to \"result.rkt\"]" (current-output-file o)))

  (define pt (make-path-tree (current-handling-directory)))
  (define ht (make-huffman-tree (path-up-for (current-handling-directory) 1) pt))
  (define ct (cleanse-huffman-tree ht))
  (define tb (huffman-tree->hash-table ht))
  
  (define temp (make-temporary-file #:base-dir (current-working-directory)))

  (define log
    (and (current-verbose-mode)
         (let ((logger (current-log-handler)))
           (lambda str-lst (void (map logger str-lst))))))

  (define-syntax-rule (prompt expr ...)
    (cond (log (log expr ...))))
  
  (prompt (format "compression ratio:~a" (analyze-compression-ratio ht))
          (format "temporary file:~a" temp))
  
  (with-handlers ((exn? (lambda (e) (delete-file temp) (raise e))))
    (call-with-output-file/lock
     #:exists 'truncate
     temp
     (lambda (out)
       (file-stream-buffer-mode out 'block)
       (define handler (make-compress-handler tb out (current-buffer-size)))
       (parameterize ((current-directory (path-up-for (current-handling-directory) 1)))
         (iter-path-tree
          (lambda (node) (call-with-values (lambda () (time-apply handler (list node)))
                                           (lambda (_ cpu real gc) (prompt (format "~a @ ~a bytes @ ~a ms[cpu] @ ~a ms[real] @ ~a ms[gc]" (file-name node) (file-size node) cpu real gc)))))
          pt)
         (handler #f))))
    (call-with-input-file/lock
      temp
      (lambda (in)
        (file-stream-buffer-mode in 'block)
        (call-with-output-file/lock
          (current-output-file)
          (lambda (fout)
            (file-stream-buffer-mode fout 'block)
            (displayln "#lang racket/base" fout)
            (displayln "#reader (submod bdnd reader)" fout)
            (s-exp->fasl (list ct pt) fout)
            (copy-port in fout)))))
    (delete-file temp)))
