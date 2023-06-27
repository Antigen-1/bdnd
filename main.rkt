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
  (require racket/fasl "interpret.rkt")
  
  (define (read-syntax _ port)
    (cond ((port-try-file-lock? port 'shared)
           (dynamic-wind
             void
             (lambda ()
               (file-stream-buffer-mode port 'block)
               (read-line port)
               (apply bdnd-interpret port (let ((r (getenv "BDND_BUFFER_SIZE"))) (and r (string->number r))) (fasl->s-exp port)))
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
      (send-generic in-buffer read
                    (lambda (n bt)
                      (cond ((eof-object? n) (check-equal? bytes b))
                            (else (work (bytes-append b (subbytes bt 0 n)))))))))

  (require "huffman.rkt" (submod "huffman.rkt" shallow))
  
  (test-case
      "huffman"
    (define tree (make-huffman-tree test-dir))
    (define ctree (cleanse-huffman-tree tree))
    (define table (huffman-tree->hash-table tree))
    (define (check byte) (check-eq? (caddr (call-with-values (lambda () (define p (hash-ref table byte)) (index-huffman-tree (cdr p) (car p) ctree)) list)) byte))
    (map check '(97 98 99 100))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  
  (require racket/cmdline raco/command-name "huffman.rkt" "lock.rkt" "buffer.rkt" racket/class racket/port racket/fasl racket/file)
  
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

  (define log
    (and (current-verbose-mode)
         (let ((logger (current-log-handler)))
           (lambda str-lst (void (map logger str-lst))))))

  (define-syntax-rule (prompt expr ...)
    (cond (log (log expr ...))))
  
  (prompt (format "compression ratio:~a" (analyze-compression-ratio ht))
          (format "temporary file:~a" temp))
  
  (with-handlers ((exn? (lambda (e)
                          (delete-directory/files #:must-exist? #f (current-output-file))
                          (delete-file temp)
                          (raise e))))
    (define fl
      (call-with-output-file/lock
        #:exists 'truncate
        temp
        (lambda (out)
          (file-stream-buffer-mode out 'block)

          (define (make-buffer %) (new % (size (cond ((current-buffer-size)) (else 1000000)))))
          
          (define in-buffer (make-buffer in-buffer%))
          (define out-buffer (make-buffer out-buffer%))

          (send-generic out-buffer set-output out)
          
          (define-values (_0 _1 filelist)
            (parameterize ((current-directory (current-handling-directory)))
              (for/fold ((r 0) (rl 0) (fl null)) ((f (in-directory)))
                (define-values (res cpu real gc)
                  (time-apply
                   (lambda ()
                     (call-with-input-file/lock
                      f
                      (lambda (in)
                        (file-stream-buffer-mode in 'block)
                        (send-generic in-buffer set-input in)
                        (define-values (_s _r _rl)
                          (let loop ((s 0) (r r) (rl rl))
                            (send-generic in-buffer read
                                          (lambda (n b)
                                            (cond ((eof-object? n) (values s r rl))
                                                  (else
                                                   (define-values (__r __rl)
                                                     (for/fold ((r r) (rl rl)) ((bt (in-bytes b 0 n)))
                                                       (define pair (hash-ref tb bt))
                                                       (let work ((r (bitwise-ior r (arithmetic-shift (cdr pair) rl))) (rl (+ (car pair) rl)))
                                                         (if (>= rl 8)
                                                             (begin (send-generic out-buffer commit (bitwise-bit-field r 0 8)) (work (arithmetic-shift r -8) (- rl 8)))
                                                             (values r rl)))))
                                                   (loop (+ s n) __r __rl)))))))
                        (values _r _rl (cons (list _s (path->string f)) fl)))))
                   null))
                (prompt (format "~a @ ~a bytes @ ~a ms[cpu] @ ~a ms[real] @ ~a ms[gc]" f (car (caaddr res)) cpu real gc))
                (apply values res))))
          (reverse filelist))))
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
            (s-exp->fasl (list fl ct (current-prefix)) fout)
            (copy-port in fout)))))
    (delete-file temp)))
