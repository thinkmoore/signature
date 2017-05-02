#lang info

(define collection "signature")
(define deps '("kw-utils"
               "racklog"
               "base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/signature.scrbl" ())))
(define pkg-desc "Syntax and utilities for writing function contracts as signatures")
(define version "0.0")
(define pkg-authors '("Scott Moore"))
