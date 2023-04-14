#lang racket/base
(define (call-with-output-file/lock file proc #:exists (exists? 'error))
  (call-with-output-file* file #:exists exists?
    (lambda (out)
      (cond ((port-try-file-lock? out 'exclusive)
             (dynamic-wind void (lambda () (file-stream-buffer-mode out 'block) (proc out)) (lambda () (port-file-unlock out))))
            (else (raise (make-exn:fail:filesystem
                          (format "fail to acquire the file lock when writing to ~a" file)
                          (current-continuation-marks))))))))

(define (call-with-input-file/lock file proc)
  (call-with-input-file* file
    (lambda (in)
      (cond ((port-try-file-lock? in 'shared)
             (dynamic-wind void (lambda () (file-stream-buffer-mode in 'block) (proc in)) (lambda () (port-file-unlock in))))
            (else (raise (make-exn:fail:filesystem
                          (format "fail to acquire the file lock when reading ~a" file)
                          (current-continuation-marks))))))))

(provide (all-defined-out))
