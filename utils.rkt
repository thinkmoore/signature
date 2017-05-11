#lang racket/base

(provide (for-syntax compare-arities
                     make-procedure-contract
                     specs->signature))

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/parse
                     syntax/to-string
                     (except-in racklog _)
                     "syntax.rkt")
         racket/contract
         racket/contract/combinator
         racket/function
         racket/list
         racket/match
         kw-utils/arity+keywords
         syntax/parse)

(define-for-syntax (compare-arities expected given fail)
  (unless (equal? (arity-mandatory expected) (arity-mandatory given))
    (fail (format "arity mismatch;~n number of mandatory arguments for instance does not match the expected number~n  expected: ~a~n  given: ~a~n"
                  (arity-mandatory expected)
                  (arity-mandatory given)))
    (unless (equal? (arity-optional expected) (arity-optional given))
      (fail (format "arity mismatch;~n number of optional arguments for instance does not match the expected number~n  expected: ~a~n  given: ~a~n"
                    (arity-optional expected)
                    (arity-optional given))))
    (for ([key (in-list (arity-mandatory-keywords expected))])
      (unless (member key (arity-mandatory-keywords given))
        (fail (format "arity mismatch;~n instance must accept mandatory keyword argument ~a~n"
                      key))))
    (for ([key (in-list (arity-mandatory-keywords given))])
      (unless (member key (arity-mandatory-keywords expected))
        (fail (format "arity mismatch;~n instance has unexpected mandatory keyword argument ~a~n"
                      key))))
    (for ([key (in-list (arity-optional-keywords expected))])
      (unless (member key (arity-optional-keywords given))
        (fail (format "arity mismatch;~n instance must accept optional keyword argument ~a~n"
                      key))))
    (for ([key (in-list (arity-optional-keywords given))])
      (unless (member key (arity-optional-keywords expected))
        (fail (format "arity mismatch;~n instance has unexpected optional keyword argument ~a~n"
                      key))))))

(define-for-syntax (plain-procedure? arity)
  (and (= 0 (length (arity-mandatory-keywords arity)))
       (= 0 (length (arity-optional-keywords arity)))))

(define-for-syntax (compute-ordering specs)
  (define fresh
    (let ([next 0])
      (lambda ()
        (begin0
          next
          (set! next (add1 next))))))
  (let/ec return
    (define %id %empty-rel)
    (define %uses %empty-rel)

    (for ([spec (in-list specs)])
      (%assert! %id () [(spec)])
      (for ([use (in-list (spec-uses spec))])
        (%assert! %uses () [(spec use)])))

    (define %before
      (%rel (left right id)
            [(left right)
             (%uses right id)
             (%is #t (free-identifier=? (spec-id left) id))]))

    (define %candidate
      (%rel (taken can)
            [(taken can)
             (%id can)
             (%not (%member can taken))]))

    (define %blocker-exists
      (%rel (taken next blockers blocker bad)
            [(taken next)
             (%set-of-1 blocker (%and (%candidate taken blocker)
                                      (%/= blocker next)
                                      (%before blocker next)) blockers)]))

    (define %next
      (%rel (taken next)
            [(taken next)
             (%candidate taken next)
             (%not (%blocker-exists taken next))]))

    (define order
      (for/fold ([taken empty])
                ([i (in-range (length specs))])
        (let ([result (%which (next) (%next taken next))])
          (unless result (return #f))
          (cons (cdar result) taken))))

    (reverse order)))

(define-for-syntax (may-be-missing? spec)
  (or (optional-spec? spec)
      (optional-keyword-spec? spec)
      (rest-spec? spec)))

(define-for-syntax (get-spec-present spec)
  (cond
    [(optional-spec? spec)
     #`(> (length args) #,(optional-spec-index spec))]
    [(optional-keyword-spec? spec)
     #`(member '#,(optional-keyword-spec-keyword spec) kws)]
    [(rest-spec? spec)
     #`(> (length args) #,(rest-spec-index spec))]
    [else #f]))

(define-for-syntax (get-spec-extracter spec)
  (cond
    [(mandatory-spec? spec)
     #`(list-ref args #,(mandatory-spec-index spec))]
    [(optional-spec? spec)
     #`(list-ref args #,(optional-spec-index spec))]
    [(mandatory-keyword-spec? spec)
     #`(list-ref kw-args (index-of kws '#,(mandatory-keyword-spec-keyword spec)))]
    [(optional-keyword-spec? spec)
     #`(list-ref kw-args (index-of kws '#,(optional-keyword-spec-keyword spec)))]
    [(rest-spec? spec)
     #`(list-tail args #,(rest-spec-index spec))]
    [(result-spec? spec)
     #`(list-ref results #,(result-spec-index spec))]))

(define-for-syntax (apply-spec-contract spec)
  (with-syntax* ([id (spec-id spec)]
                 [present? (get-spec-present spec)]
                 [extracted (get-spec-extracter spec)]
                 [contracted (if (spec-contract spec)
                                 #`(((contract-late-neg-projection #,(spec-contract spec))
                                     (blame-add-context partial-blame
                                                        #,(if (argument-spec? spec)
                                                              (format "the ~a argument of" (syntax->datum (spec-id spec)))
                                                              (format "the ~a result of" (syntax->datum (spec-id spec))))
                                                        #:swap? #,(argument-spec? spec)))
                                    extracted party)
                                 #`extracted)])
    (if (may-be-missing? spec)
        #`(id (if present? contracted the-unsupplied-arg))
        #`(id contracted))))

(define-for-syntax (make-sorted-args specs)
  (define mandatory-args
    (sort (filter mandatory-spec? specs)
          <
          #:key mandatory-spec-index))
  (define optional-args
    (sort (filter optional-spec? specs)
          <
          #:key optional-spec-index))
  (define rest-arg
    (let ([filtered (filter rest-spec? specs)])
      (and (not (empty? filtered)) (first filtered))))
  #`(filter (lambda (val) (not (unsupplied-arg? val)))
            (list* #,@(map spec-id mandatory-args)
                   #,@(map spec-id optional-args)
                   #,(if rest-arg (spec-id rest-arg) #'empty))))

(define-for-syntax (make-sorted-keyword-args specs)
  (define keyword-specs
    (sort (filter (lambda (spec) (or (mandatory-keyword-spec? spec)
                                     (optional-keyword-spec? spec)))
                  specs)
          keyword<?
          #:key (lambda (spec)
                  (if (mandatory-keyword-spec? spec)
                      (mandatory-keyword-spec-keyword spec)
                      (optional-keyword-spec-keyword spec)))))
  #`(filter (lambda (val) (not (unsupplied-arg? val)))
            (list #,@(map spec-id keyword-specs))))

(define-for-syntax (make-sorted-results specs)
  (define sorted
    (sort specs < #:key result-spec-index))
  (map spec-id sorted))

(define (make-check-arity fail mandatory optional mandatory-kw optional-kw spec-results)
  (lambda (value)
    (and (procedure? value)
         (let* ([arity+keywords (procedure-arity+keywords value)]
                [arity          (normalize-arity (arity+keywords-arity arity+keywords))]
                [results        (let ([result-arity (procedure-result-arity value)])
                                  (and result-arity (normalize-arity result-arity)))]
                [required       (arity+keywords-required-kws arity+keywords)]
                [allowed        (arity+keywords-allowed-kws arity+keywords)])
           (define mandatory-arguments
             (match arity
               [(? integer? i) i]
               [(arity-at-least i) i]
               [(cons i rest) i]))
           (define optional-arguments
             (match arity
               [(? integer? i) 0]
               [(arity-at-least i) 0]
               [list
                (match (last list)
                  [(? integer? i)     (- i mandatory-arguments)]
                  [(arity-at-least i) (- i mandatory-arguments)])]))
           (define rest-arguments
             (match arity
               [(? integer? i) #f]
               [(arity-at-least i) #t]
               [list
                (match (last list)
                  [(? integer? i) #f]
                  [(arity-at-least i) #t])]))

           ; check mandatory arguments
           (unless (= mandatory-arguments mandatory)
             (fail (if (= 1 mandatory)
                       "procedure that accepts 1 mandatory argument"
                       (format "procedure that accepts ~a mandatory arguments" mandatory))))
           ; check optional arguments
           (if optional
               (unless (and (= optional-arguments optional) (not rest-arguments))
                 (fail (if (= 1 optional)
                           "procedure that accepts 1 optional argument"
                           (format "procedure that accepts ~a optional arguments" optional))))
               (unless rest-arguments
                 (fail "procedure that accepts an arbitrary number of arguments")))
           ; check mandatory keyword arguments
           (for ([kw (in-list mandatory-kw)])
             (unless (member kw required)
               (fail (format "procedure that requires keyword ~a" kw))))
           (for ([kw (in-list required)])
             (unless (member kw mandatory-kw)
               (fail (format "procedure that does not require keyword ~a" kw))))
           ; check optional keyword arguments
           (when allowed
             (for ([kw (in-list optional-kw)])
               (unless (member kw allowed)
                 (fail (format "procedure that allows keyword ~a" kw)))))
           ; check results
           (unless (or (not results) (= results spec-results))
             (fail (if (= 1 spec-results)
                       "procedure that returns 1 result"
                       (format "procedure that returns ~a results" spec-results))))
           #t))))

(define-for-syntax (spec->signature spec)
  (match spec
    [(mandatory-spec id uses ctc index)
     (if ctc #`((#,id : #,ctc)) #`(#,id))]
    [(optional-spec id uses ctc index)
     (if ctc #`(([#,id] : #,ctc)) #`([#,id]))]
    [(mandatory-keyword-spec id uses ctc keyword)
     (if ctc #`(#,keyword (#,id : #,ctc)) #`(#,keyword #,id))]
    [(optional-keyword-spec id uses ctc keyword)
     (if ctc #`(#,keyword ([#,id] : #,ctc)) #`(#,keyword [#,id]))]
    [(rest-spec id uses ctc index)
     (if ctc #`((#,id : #,ctc)) #`(#,id))]
    [(result-spec id uses ctc index)
     (if ctc #`((#,id : #,ctc)) #`(#,id))]))

(define-for-syntax (specs->signature specs)
  (let ([mandatory (sort (filter mandatory-spec? specs)
                         < #:key mandatory-spec-index)]
        [optional  (sort (filter optional-spec? specs)
                         < #:key optional-spec-index)]
        [mandatory-keyword (filter mandatory-keyword-spec? specs)]
        [optional-keyword (filter optional-keyword-spec? specs)]
        [rest (let ([result (filter rest-spec? specs)])
                (if (empty? result)
                    #f
                    (first result)))]
        [result (sort (filter result-spec? specs)
                      < #:key result-spec-index)])
    (if rest
        #`(#,@(apply append-syntax (map spec->signature mandatory))
           #,@(apply append-syntax (map spec->signature optional))
           #,@(apply append-syntax (map spec->signature mandatory-keyword))
           #,@(apply append-syntax (map spec->signature optional-keyword))
           ..
           #,@(spec->signature rest)
           ->
           #,@(apply append-syntax (map spec->signature result)))
        #`(#,@(apply append-syntax (map spec->signature mandatory))
           #,@(apply append-syntax (map spec->signature optional))
           #,@(apply append-syntax (map spec->signature mandatory-keyword))
           #,@(apply append-syntax (map spec->signature optional-keyword))
           ->
           #,@(apply append-syntax (map spec->signature result))))))

(define-for-syntax (make-procedure-contract name stx fail)
  (syntax-parse stx
    [(signature:sig)
     (let ([argument-order (compute-ordering (filter argument-spec? (attribute signature.specs)))]
           [result-order   (compute-ordering (filter result-spec? (attribute signature.specs)))])
       (unless (and argument-order result-order)
         (fail "cycle in dependent contracts"))
       (with-syntax ([mandatory    (arity-mandatory (attribute signature.arity))]
                     [optional     (arity-optional (attribute signature.arity))]
                     [mandatory-kw #`'#,(arity-mandatory-keywords (attribute signature.arity))]
                     [optional-kw  #`'#,(arity-optional-keywords (attribute signature.arity))]
                     [spec-results  (arity-results (attribute signature.arity))])
         #`(make-contract
            #:name '#,name
            #:first-order
            (lambda (value)
              (let/ec return ((make-check-arity (lambda (expected) (return #f))
                                                mandatory optional
                                                mandatory-kw optional-kw
                                                spec-results) value)))
            #:late-neg-projection
            (lambda (partial-blame)
              (lambda (value party)
                ((make-check-arity (lambda (expected)
                                     (raise-blame-error (blame-add-missing-party partial-blame party)
                                                        value
                                                        (list 'expected: expected
                                                              'given: "~e")
                                                        value))
                                   mandatory optional
                                   mandatory-kw optional-kw
                                   spec-results) value)
                (impersonate-procedure
                 value
                 (make-keyword-procedure
                  (lambda (kws kw-args . args)
                    (letrec (#,@(map apply-spec-contract argument-order))
                      (define (result-handler . results)
                        (unless (= (length results) #,(arity-results (attribute signature.arity)))
                          (raise-blame-error (blame-add-missing-party (blame-add-context partial-blame
                                                                                         "the result of")
                                                                      party)
                                             value
                                             (list 'expected (if (= 1 #,(arity-results (attribute signature.arity)))
                                                                 "1 result"
                                                                 (format "~a results" (length results)))
                                                   'given "~e")
                                             results))
                        (letrec (#,@(map apply-spec-contract result-order))
                          (values #,@(make-sorted-results result-order))))
                      (apply values result-handler #,(make-sorted-keyword-args argument-order) #,(make-sorted-args argument-order))))
                  (lambda args
                    (letrec (#,@(map apply-spec-contract
                                     (filter (lambda (spec) (or (mandatory-spec? spec)
                                                                (optional-spec? spec)
                                                                (rest-spec? spec)))
                                             argument-order)))
                      (define (result-handler . results)
                        (unless (= (length results) #,(arity-results (attribute signature.arity)))
                          (raise-blame-error (blame-add-missing-party (blame-add-context partial-blame
                                                                                         "the result of")
                                                                      party)
                                             value
                                             (list 'expected (if (= 1 #,(arity-results (attribute signature.arity)))
                                                                 "1 result"
                                                                 (format "~a results" (length results)))
                                                   'given "~e")
                                             results))
                        (letrec (#,@(map apply-spec-contract result-order))
                          (values #,@(make-sorted-results result-order))))
                      (apply values result-handler #,(make-sorted-args argument-order)))))))))))]))
