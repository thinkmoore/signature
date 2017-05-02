#lang scribble/manual
@require[@for-label[signature
                    racket/base]]

@title{signature}
@author{Scott Moore}

@defmodule[signature]

Syntax classes and utilities for defining function signatures with contracts, including
an alternative syntax for indy-dependent contracts.

@defform*/subs[#:literals (: ..)
[(--> argument ... optional-rest results)]
([argument id+ctc
           (code:line keyword id+ctc)]
 [id+ctc   id-or-optional
           (id-or-optional : contract-expr)]
 [id-or-optional id
                 [id]]
 [optional-rest (code:line)
  (code:line .. id+ctc)]
 [results result
          (result ...)]
 [result (id+ctc)])
]{

The @racket[-->] defines an indy-dependent contract like @racket[->i].  Unlike @racket[->i],
@racket[-->] does not require contract expressions to declare dependencies on other arguments.
Instead, it infers them automatically using the names of arguments and result values declared
in the signature.

For example, the contract:
@racketblock[(->i ([x number?]
                   [y (x) (>=/c x)])
                  [result (x y) (and/c number? (>=/c (+ x y)))])]
can be written as the following.
@racketblock[(--> (x : number?) (y : (>=/c x))
                  (result : (and/c number? (>=/c (+ x y)))))]

Unlike @racket[->i], mandatory and optional arguments are not specified separately.
The contract
@racketblock[(->i (#:x [x number?])
                  (#:y [y (x) (>=/c x)])
                  [result (x y) (and/c number? (>=/c (+ x y)))])]
can be written as the following.
@racketblock[(--> #:x (x : number?) #:y ([y] : (>=/c x))
                  (result : (and/c number? (>=/c (+ x y)))))]

Like @racket[->i], the contract expressions are not always evaluated in
order. If there are optional arguments that are not supplied, then
the corresponding variables will be bound to a special value
called @racket[the-unsupplied-arg] value.
}

@defform*/subs[#:literals (: ..)
[(define/sig (id (argument ... optional-rest) (result ...))
   body ...)]
([argument id+ctc
           (code:line keyword id+ctc)]
 [id+ctc   id-or-optional/default
           (id-or-optional : contract-expr)]
 [id-or-optional/default id
                 [id default-expr]]
 [optional-rest (code:line)
  (code:line .. id+ctc)]
 [results result
          (result ...)]
 [result (id+ctc)])
]{

Defines a procedure with the given signature and applies the corresponding contract.

For example,
@racketblock[(define/sig (add #:x (x : number?) #:y ([y x] : (>=/c x)) (result : (and/c number? (>=/c (+ x y)))))
               (+ x y))]
is equivalent to the following.
@racketblock[(define/contract (add #:x x #:y [y x])
               (--> #:x (x : number?) #:y ([y] : (>=/c x)) (result : (and/c number? (>=/c (+ x y)))))
               (+ x y))]
}