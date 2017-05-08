#lang racket/base

(provide --> define/sig)

(require (for-syntax racket/base
                     syntax/parse
                     "syntax.rkt")
         racket/contract
         "utils.rkt")

(define-syntax (--> stx)
  (syntax-parse stx
    [(_ signature:sig)
     (make-procedure-contract stx #'signature error)]))

(define-syntax (define/sig stx)
  (syntax-parse stx
    [(_ (name:id signature:sig/defaults) body ...+)
     #`(define/contract (name #,@(attribute signature.decl))
         (--> #,@(attribute signature.sig))
         body ...)]))

(module* test racket/base
  (require racket/contract
           (submod ".."))

  (require rackunit)
  
  (define/contract (foo x [y x] #:foo foo . rest)
    (--> (x ([y] : (lambda (y) (equal? x y))) #:foo foo .. (rest : (listof integer?)))
         ((z : integer?)))
    x)

  (define/sig (foo2 (x (y : (lambda (y) (equal? x y))) #:foo foo .. (rest : (listof integer?)))
                    ((z : integer?)))
    x)

  (define/sig (foo3 (x (y : (lambda (y) (equal? x y))) #:foo foo .. (rest : (listof integer?)))
                    ((z : integer?)))
    (values x x))

  (check-not-exn (lambda () (foo 1 1 #:foo 1 1)))
  (check-exn exn:fail:contract? (lambda () (foo 1 2 #:foo 1 1)))
  (check-exn exn:fail:contract? (lambda () (foo 1 1 #:foo 1 'a)))
  (check-not-exn (lambda () (foo2 1 1 #:foo 1 1)))
  (check-exn exn:fail:contract? (lambda () (foo2 1 2 #:foo 1 1)))
  (check-exn exn:fail:contract? (lambda () (foo2 1 1 #:foo 1 'a)))
  (check-exn exn:fail:contract? (lambda () (foo3 1 1 #:foo 1 1))))

(module defs racket
  (provide (contract-out [test-mod (->i ([f (-> integer? integer?)]
                                         [x (f) f])
                                        ()
                                        [result integer?])]))
  (define (test-mod f x)
    (f x)))

(require (submod "." defs))

(define/contract (test f x)
  (->i ([f (-> integer? integer?)]
        [x (f) f])
       ()
       [result integer?])
  (f x))

(define/sig (test-sig ((f : (-> integer? integer?)) (x : f)) ((result : integer?)))
  (f x))
