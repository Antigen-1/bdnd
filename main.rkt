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
  (require racket/fasl racket/syntax "interpret.rkt")
  
  (define (read-syntax src port)
    (cond ((port-try-file-lock? port 'shared)
           (dynamic-wind
             void
             (lambda ()
               (file-stream-buffer-mode port 'block)
               (read-line port)
               (bdnd-interpret (fasl->s-exp port) (fasl->s-exp port) (fasl->s-exp port) port (let ((r (getenv "BDND_BUFFER_SIZE"))) (and r (string->number r))))
               (datum->syntax #f (list 'module (generate-temporary 'bdnd) 'racket/base)))
             (lambda () (port-file-unlock port))))
          (else (raise (make-exn:fail:filesystem
                        "fail to acquire the file lock when reading the source file"
                        (current-continuation-marks))))))
  
  (provide read-syntax))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (require "buffer.rkt" racket/random racket/class)

  (test-case
      "buffer"
    (define in-buffer (new in-buffer% (size 1000)))
    (define out-buffer (new out-buffer% (size 1000)))
    (define-values (in out) (make-pipe))
    (send-generic in-buffer set-input in)
    (send-generic out-buffer set-output out)
    (define bytes (crypto-random-bytes 1000000))
    (for ((b (in-bytes bytes)))
      (send-generic out-buffer commit b))
    (send-generic out-buffer flush)
    (close-output-port out)
    (let work ((b #""))
      (define v (send-generic in-buffer read))
      (cond ((eof-object? v) (check-equal? bytes b))
            (else (work (bytes-append b v))))))

  (require racket/async-channel "codec.rkt")
  
  (test-case
      "codec"
    (define-values (in out) (make-pipe))
    (define-values (ch1 thd) (compress-to-port out 10))
    (define-values (ch2 _) (decompress-from-port in 10))
    (define bit-list '(0 1 1 0 1 0 1 1 1))
    (async-channel-put ch1 bit-list)
    (async-channel-put ch1 #f)
    (sync (handle-evt thd (lambda (_) (close-output-port out))))
    (check-equal? (sync ch2) '(0 1 1 0 1 0 1 1))
    (check-equal? (sync ch2) '(1 0 0 0 0 0 0 0)))

  (require "huffman.rkt")
  
  (test-case
      "huffman"
    (define test-file (build-path test-dir "huffman"))
    (define tree (make-huffman-tree test-file))
    (define ctree (cleanse-huffman-tree tree))
    (define table (huffman-tree->hash-table tree))
    (check-equal? (consult-huffman-tree 97 table) '(0))
    (check-equal? (consult-huffman-tree 98 table) '(1 0 1))
    (check-eq? (foldl (lambda (ins tree) (index-huffman-tree ins tree)) ctree (consult-huffman-tree 99 table)) 99)
    (check-eq? (foldl (lambda (ins tree) (index-huffman-tree ins tree)) ctree (consult-huffman-tree 100 table)) 100)))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  
  (require racket/cmdline raco/command-name "huffman.rkt" "codec.rkt" "lock.rkt" "buffer.rkt" racket/class racket/async-channel racket/port racket/fasl racket/file)
  
  (define current-prefix (make-parameter "file"))
  (define current-output-file (make-parameter "result.rkt"))
  (define current-handling-directory (make-parameter #f))
  (define current-buffer-size (make-parameter (let ((r (getenv "BDND_BUFFER_SIZE"))) (and r (string->number r)))))
  (define current-verbose-mode (make-parameter #f))
  (define current-log-handler (make-parameter displayln))

  (command-line #:program (short-program+command-name)
                #:once-any (("-b" "--buffer") b "specify the size of the buffer"
                                              (cond ((string->number b) => current-buffer-size)))
                #:once-any (("-v" "--verbose") "increase verbosity"
                                               (current-verbose-mode #t))
                #:once-any (("-l" "--log") "report information at the `info` level with the topic `bdnd`"
                                           (current-log-handler (lambda (s) (log-message (current-logger) 'info 'bdnd s))))
                #:once-any (("-d" "--directory") d "specify a directory" (current-handling-directory d))
                #:once-any (("-p" "--prefix") p "specify the prefix[default to \"file\"]" (current-prefix p))
                #:once-any (("-o" "--output") o "specify the output file[default to \"result.rkt\"]" (current-output-file o)))
  
  (define ht (make-huffman-tree (current-handling-directory)))
  (define ct (cleanse-huffman-tree ht))
  (define tb (huffman-tree->hash-table ht))
  
  (define temp (make-temporary-file))

  (define-syntax-rule (prompt str ...)
    (cond ((current-verbose-mode) ((current-log-handler) str) ...)))

  (prompt (format "compression ratio:~a" (analyze-compression-ratio ht))
          (format "temporary file:~a" temp))
  
  (with-handlers ((exn:fail:filesystem? (lambda (e) (delete-directory/files #:must-exist? #f (current-output-file)) (raise e))))
    (define fl
      (call-with-output-file/lock
        #:exists 'truncate/replace
        temp
        (lambda (out)
          (file-stream-buffer-mode out 'block)
          (define-values (och thd) (compress-to-port out (cond ((current-buffer-size)) (else 1000000))))
          (define buffer (new in-buffer% (size (cond ((current-buffer-size)) (else 1000000)))))
          (define filelist
            (parameterize ((current-directory (current-handling-directory)))
              (for/fold ((r null)) ((f (in-directory)))
                (cond ((file-exists? f)
                       (collect-garbage 'incremental)
                       (define start (current-milliseconds))
                       (call-with-input-file/lock
                         f
                         (lambda (in)
                           (file-stream-buffer-mode in 'block)
                           (send-generic buffer set-input in)
                           (cons
                            (list
                             (let loop ((s 0))
                               (send-generic buffer read
                                             (lambda (n b)
                                               (cond ((eof-object? n) (prompt (format "~a @ ~a bytes @ ~a ms" f s (- (current-milliseconds) start))) s)
                                                     (else
                                                      (collect-garbage 'incremental)
                                                      (let work ((i 0))
                                                        (cond ((= i n) (loop (+ s n)))
                                                              (else (async-channel-put och (consult-huffman-tree (bytes-ref b i) tb))
                                                                    (work (add1 i))))))))))
                             (path->string f))
                            r))))
                      (else r)))))
          (async-channel-put och #f)
          (sync thd)
          (reverse filelist))))
    (call-with-input-file/lock
      temp
      (lambda (in)
        (file-stream-buffer-mode in 'block)
        (call-with-output-file/lock
          (current-output-file)
          (lambda (fout)
            (file-stream-buffer-mode fout 'block)
            (displayln "#lang bdnd" fout)
            (s-exp->fasl fl fout)
            (s-exp->fasl ct fout)
            (s-exp->fasl (current-prefix) fout)
            (copy-port in fout)))))))
