; SPDX-FileCopyrightText: 2021 Jelle Licht <jlicht@posteo.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (guix)
  #:export (laura))

(use-modules (ice-9 match)
	     (ice-9 popen)
	     (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26)
             ((guix build utils) #:select (with-directory-excursion))
	     (guix gexp)
	     (guix packages)
             ((guix licenses) #:prefix license:)
             (guix build-system gnu)
	     (guix transformations)
	     (guix utils)
	     (gnu packages autotools)
	     (gnu packages base)
	     (gnu packages guile)
	     (gnu packages package-management)
	     (gnu packages pkg-config)
	     )


(define %source-dir (dirname (current-filename)))

(define git-file?
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "ls-files")))
         (files (let loop ((lines '()))
                  (match (read-line pipe)
                    ((? eof-object?)
                     (reverse lines))
                    (line
                     (loop (cons line lines))))))
         (status (close-pipe pipe)))
    (lambda (file stat)
      (match (stat:type stat)
        ('directory
         #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_
         #f)))))


(define-public laura
  (package
    (name "laura")
    (version "0.1.0")
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #false		; for reproducibility
       #:make-flags
       '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     (let ((p (package-input-rewriting
               `((,guile-3.0 . ,guile-3.0-latest))
               #:deep? #false)))
       `(("guix" ,guix)
         ("guile" ,guile-3.0-latest))))
    (home-page "https://github.com/jellelicht/laura")
    (synopsis "Lorri for GNU Guix")
    (description "Laura aims to bring a fire-and-forget, direnv
integrated way of working to GNU Guix.")
    (license (list license:gpl3+ license:agpl3+ license:cc0))))

laura
