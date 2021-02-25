(define-module (laura utils)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 pretty-print)
  #:export (laura-configuration
            laura-configuration?
            laura-sync
            laura-init
            laura-envrc))


(define (invoke* . args)
  "Invoke PROGRAM with the given ARGS.  Raise an exception
if the exit code is non-zero; otherwise return #t."
  (define &invoke-error (@@ (guix build utils) &invoke-error))
  (let ((code (system (string-join args))))
    (unless (zero? code)
      (raise (condition (&invoke-error
                         (program (car args))
                         (arguments (cdr args))
                         (exit-status (status:exit-val code))
                         (term-signal (status:term-sig code))
                         (stop-signal (status:stop-sig code))))))
    #t))


(define-record-type* <laura-configuration>
  laura-configuration make-laura-configuration
  laura-configuration?
  (manifest-file laura-configuration-manifest-file
		 (default "manifest.scm"))
  (profile-expiration laura-configuration-expiration
	              (default (* 10 24 3600))) ; 10 days
  (profile laura-configuration-profile
	   (default ".guix-profile"))
  (channel-file laura-configuration-channel-file
	        (default #f)))

(define default-laura-literal '(laura-configuration))
(define default-manifest-literal '(";; -*- geiser-scheme-implementation: guile -*-"
                                   (use-modules (gnu)
	                                        (guix profiles)
	                                        (guix packages)
	                                        (gnu packages base))
                                   (packages->manifest (list hello))))

(define (laura-envrc config)
  (define profile
    (laura-configuration-profile config))
  (format #f "eval $(guix package --profile=$PWD/~a --search-paths=prefix)\n" profile))

(define (laura-sync config)
  (define manifest
    (laura-configuration-manifest-file config))
  (define expiration
    (laura-configuration-expiration config))
  (define profile
    (laura-configuration-profile config))
  (define channel-file
    (laura-configuration-channel-file config))
  (define (timestamp)
    (date->string (time-utc->date (current-time time-utc))
                  "[~4]"))

  (format #t "~a starting upgrade...~%" (timestamp))
  (guard (c ((invoke-error? c)
             (report-invoke-error c)))
    (let ((cmd `("guix" ,@(if channel-file
                              (list "time-machine"
                                    (string-append "--channels=" channel-file)
                                    "--")
                              '())
                 "package"
                 ,(string-append "--profile=" profile)
                 ,(string-append "--manifest=" manifest))))
      ;; (format #t "cmd is: ~a\n" cmd)
      ;; (sigaction SIGINT SIG_DFL)
      (sigaction SIGINT
        (lambda args
          (display "hello!")
          (exit 1)))
      (apply invoke* cmd))
    (guard (c ((invoke-error? c)
               (report-invoke-error c)))
      (invoke* "guix" "package"
              (string-append "--profile=" profile)
              (string-append "--delete-generations=" (number->string expiration) "s")))
    (format #t "~a upgrade complete~%" (timestamp))))

(define (laura-init laura-file loader)
  (define (default-laura)
    (call-with-output-file laura-file
      (cut pretty-print default-laura-literal <>)))
  (define (default-manifest manifest)
    (call-with-output-file manifest
      (lambda (port)
        (for-each (cut pretty-print <> port #:display? #t)
                  default-manifest-literal))))
  (define (default-envrc cfg)
    (call-with-output-file ".envrc"
      (cut display (laura-envrc cfg) <>)))
  (unless (file-exists? laura-file)
    (default-laura))
  (let* ((cfg (loader laura-file))
         (manifest (laura-configuration-manifest-file cfg))
         (profile (laura-configuration-profile cfg)))
    (unless (file-exists? manifest)
      (default-manifest manifest))
    (laura-sync cfg)
    (if (file-exists? ".envrc")
        (format #t ".envrc already exists, please run `guix laura envrc' for further instructions.\n")
        (default-envrc cfg))))
