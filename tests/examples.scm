; SPDX-FileCopyrightText: 2021 Jelle Licht <jlicht@posteo.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (test-examples)
  #:use-module (srfi srfi-64))

(test-begin "examples")

(test-equal "obvious" 2 2)

(test-end "examples")
