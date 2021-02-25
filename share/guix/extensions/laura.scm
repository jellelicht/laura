(define-module (guix extensions laura)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (laura utils)
  #:use-module (ice-9 match)
  #:export (guix-laura))

(define %default-file "laura.scm")

(define wut (laura-configuration))

(define (load-channels file)
  (let ((result (load* file (make-user-module '((laura utils))))))
    (if (laura-configuration? result)
	result
	(leave (G_ "'~a' did not return a laura-configuration~%") file))))

(define-command (guix-laura . args)
  (category development)
  (synopsis "easily managed environments")
  (match args
    (("init")
     (laura-init %default-file load-channels))
    (_
     (let ((ll (load-channels %default-file)))
       (match args
         (("envrc")
          (format #t "Put the following in .envrc:\n ~a"
                  (laura-envrc ll)))
         (("sync")
          (laura-sync ll)))))))
