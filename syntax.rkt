#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/list
         syntax/parse)

(struct arity (mandatory optional mandatory-keywords optional-keywords results)
  #:prefab)
  
(define (append-syntax . syntax-lists)
  (apply append (map syntax->list syntax-lists)))

(define-syntax-class id-not-reserved
  (pattern id:id
           #:when (not (member (syntax->datum #'id) '(: ..)))))

(define-splicing-syntax-class arg
  #:datum-literals (: ..)
  #:attributes (mandatory-id mandatory-ctc optional-id optional-ctc
                mandatory-kw mandatory-kw-id mandatory-kw-ctc
                optional-kw optional-kw-id optional-kw-ctc
                decl invoc)
  (pattern mandatory-id:id-not-reserved
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-id)
           #:attr invoc #'(mandatory-id))
  (pattern (mandatory-id:id-not-reserved : mandatory-ctc:expr)
           #:attr optional-id #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-id)
           #:attr invoc #'(mandatory-id))
  (pattern [optional-id:id-not-reserved]
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-ctc #f
           #:attr decl #'([optional-id the-unsupplied-arg])
           #:attr invoc #'(optional-id))
  (pattern ([optional-id:id-not-reserved] : optional-ctc:expr)
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-ctc #f
           #:attr decl #'([optional-id the-unsupplied-arg])
           #:attr invoc #'(optional-id))
  (pattern (~seq mandatory-kw:keyword mandatory-kw-id:id-not-reserved)
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-ctc #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-kw mandatory-kw-id)
           #:attr invoc #'(mandatory-kw mandatory-kw-id))
  (pattern (~seq mandatory-kw:keyword (mandatory-kw-id:id-not-reserved : mandatory-kw-ctc:expr))
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-kw mandatory-kw-id)
           #:attr invoc #'(mandatory-kw mandatory-kw-id))
  (pattern (~seq optional-kw:keyword [optional-kw-id:id-not-reserved])
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(optional-kw [optional-kw-id the-unsupplied-arg])
           #:attr invoc #'(optional-kw optional-kw-id))
  (pattern (~seq optional-kw:keyword ([optional-kw-id:id-not-reserved] : optional-kw-ctc:expr))
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr decl #'(optional-kw [optional-kw-id the-unsupplied-arg])
           #:attr invoc #'(optional-kw optional-kw-id)))

(define-syntax-class rest-arg
  #:datum-literals (: ..)
  #:attributes (id ctc)
  (pattern id:id-not-reserved
           #:attr ctc #f)
  (pattern (id:id-not-reserved : ctc:expr)))

(define-splicing-syntax-class arg/defaults
  #:datum-literals (: ..)
  #:attributes (mandatory-id mandatory-ctc
                optional-id optional-default optional-ctc
                mandatory-kw mandatory-kw-id mandatory-kw-ctc
                optional-kw optional-kw-id optional-kw-default optional-kw-ctc
                decl sig invoc)
  (pattern mandatory-id:id-not-reserved
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-default #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-default #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-id)
           #:attr sig #'(mandatory-id)
           #:attr invoc #'(mandatory-id))
  (pattern (mandatory-id:id-not-reserved : mandatory-ctc:expr)
           #:attr optional-id #f
           #:attr optional-default #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-default #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-id)
           #:attr sig #'((mandatory-id : mandatory-ctc))
           #:attr invoc #'(mandatory-id))
  (pattern [optional-id:id-not-reserved optional-default:expr]
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-default #f
           #:attr optional-kw-ctc #f
           #:attr decl #'((optional-id optional-default))
           #:attr sig #'((optional-id))
           #:attr invoc #'(optional-id))
  (pattern ([optional-id:id-not-reserved optional-default:expr] : optional-ctc:expr)
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-default #f
           #:attr optional-kw-ctc #f
           #:attr decl #'((optional-id optional-default))
           #:attr sig #'(((optional-id) : optional-ctc))
           #:attr invoc #'(optional-id))
  (pattern (~seq mandatory-kw:keyword mandatory-kw-id:id-not-reserved)
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-default #f
           #:attr optional-ctc #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-default #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-kw mandatory-kw-id)
           #:attr sig #'(mandatory-kw mandatory-kw-id)
           #:attr invoc #'(mandatory-kw mandatory-kw-id))
  (pattern (~seq mandatory-kw:keyword (mandatory-kw-id:id-not-reserved : mandatory-kw-ctc:expr))
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-default #f
           #:attr optional-ctc #f
           #:attr optional-kw #f
           #:attr optional-kw-id #f
           #:attr optional-kw-default #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(mandatory-kw mandatory-kw-id)
           #:attr sig #'(mandatory-kw (mandatory-kw-id : mandatory-kw-ctc))
           #:attr invoc #'(mandatory-kw mandatory-kw-id))
  (pattern (~seq optional-kw:keyword [optional-kw-id:id-not-reserved optional-kw-default:expr])
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-default #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr optional-kw-ctc #f
           #:attr decl #'(optional-kw (optional-kw-id optional-kw-default))
           #:attr sig #'(optional-kw (optional-kw-id))
           #:attr invoc #'(optional-kw optional-kw-id))
  (pattern (~seq optional-kw:keyword ([optional-kw-id:id-not-reserved optional-kw-default:expr] : optional-kw-ctc:expr))
           #:attr mandatory-id #f
           #:attr mandatory-ctc #f
           #:attr optional-id #f
           #:attr optional-default #f
           #:attr optional-ctc #f
           #:attr mandatory-kw #f
           #:attr mandatory-kw-id #f
           #:attr mandatory-kw-ctc #f
           #:attr decl #'(optional-kw (optional-kw-id optional-kw-default))
           #:attr sig #'(optional-kw ([optional-kw-id] : optional-kw-ctc))
           #:attr invoc #'(optional-kw optional-kw-id)))

(define-syntax-class result
  #:datum-literals (: ..)
  #:attributes (id ctc)
  (pattern id:id-not-reserved
           #:attr ctc #f)
  (pattern (id:id-not-reserved : ctc:expr)))

(define (use-set-union left right)
  (for/fold ([uses left])
            ([use (in-list right)])
    (if (not (member use uses free-identifier=?))
        (cons use uses)
        uses)))

(define (get-expr-uses ids expr)
  (if expr
      (syntax-parse expr
        [id:id
         (if (member #'id ids free-identifier=?)
             (list #'id)
             empty)]
        [(e ...)
         (for/fold ([uses empty])
                   ([use-set (in-list (map (lambda (e) (get-expr-uses ids e)) (syntax->list #'(e ...))))])
           (use-set-union uses use-set))])
      empty))

(define (duplicate-ids? id-list)
  (let/ec return
    (for/fold ([seen empty])
              ([id (in-list (filter identity id-list))])
      (when (member id seen free-identifier=?)
        (return #t))
      (cons id seen))
    #f))

(struct spec (id uses contract))
(struct mandatory-spec spec (index))
(struct optional-spec spec (index))
(struct mandatory-keyword-spec spec (keyword))
(struct optional-keyword-spec spec (keyword))
(struct rest-spec spec (index))
(struct result-spec spec (index))

(define (argument-spec? spec)
  (not (result-spec? spec)))

(define (make-argument-specs ctor ids arg-ids contracts)
  (filter identity
          (for/list ([(id index) (in-indexed (in-list arg-ids))]
                     [ctc (in-list contracts)])
            (and id
                 (ctor id (get-expr-uses ids ctc) ctc index)))))

(define (make-rest-spec ids id ctc index)
  (rest-spec id (get-expr-uses ids ctc) ctc index))

(define (make-keyword-specs ctor ids keywords keyword-ids contracts)
  (filter identity
          (for/list ([id (in-list keyword-ids)]
                     [keyword (in-list keywords)]
                     [ctc (in-list contracts)])
            (and id
                 (ctor id (get-expr-uses ids ctc) ctc keyword)))))

(define-splicing-syntax-class sig
  #:datum-literals (: ..)
  #:attributes (arity specs decl invoc)
  (pattern (~seq (arg:arg ...) (result:result ...))
           #:attr arity (arity (length (filter identity (attribute arg.mandatory-id)))
                               (length (filter identity (attribute arg.optional-id)))
                               (map syntax->datum (filter identity (attribute arg.mandatory-kw)))
                               (map syntax->datum (filter identity (attribute arg.optional-kw)))
                               (length (attribute result)))
           #:with ids (filter identity (append (attribute arg.mandatory-id)
                                               (attribute arg.optional-id)
                                               (attribute arg.mandatory-kw-id)
                                               (attribute arg.optional-kw-id)
                                               (attribute result.id)))
           #:attr specs (append (make-argument-specs mandatory-spec
                                                     (syntax->list #'ids)
                                                     (attribute arg.mandatory-id)
                                                     (attribute arg.mandatory-ctc))
                                (make-argument-specs optional-spec
                                                     (syntax->list #'ids)
                                                     (attribute arg.optional-id)
                                                     (attribute arg.optional-ctc))
                                (make-keyword-specs mandatory-keyword-spec
                                                    (syntax->list #'ids)
                                                    (attribute arg.mandatory-kw)
                                                    (attribute arg.mandatory-kw-id)
                                                    (attribute arg.mandatory-kw-ctc))
                                (make-keyword-specs optional-keyword-spec
                                                    (syntax->list #'ids)
                                                    (attribute arg.optional-kw)
                                                    (attribute arg.optional-kw-id)
                                                    (attribute arg.optional-kw-ctc))
                                (make-argument-specs result-spec
                                                     (syntax->list #'ids)
                                                     (attribute result.id)
                                                     (attribute result.ctc)))
           #:attr decl #`(#,@(apply append-syntax (attribute arg.decl)))
           #:attr invoc #`(#,@(apply append-syntax (attribute arg.invoc)))
           #:fail-when (duplicate-ids? (syntax->list #'ids))
           "duplicate identifiers in signature"
           #:fail-when (let ([result-ids (map spec-id (filter result-spec? (attribute specs)))]
                             [arg-uses   (map spec-uses (filter argument-spec? (attribute specs)))])
                         (ormap (lambda (use-set)
                                  (ormap (lambda (result-id)
                                           (member result-id use-set free-identifier=?))
                                         result-ids))
                                arg-uses))
           "argument contracts cannot refer to results")
  (pattern (~seq (arg:arg ... .. rest:rest-arg) (result:result ...))
           #:attr arity (arity (length (filter identity (attribute arg.mandatory-id)))
                               #f
                               (map syntax->datum (filter identity (attribute arg.mandatory-kw)))
                               (map syntax->datum (filter identity (attribute arg.optional-kw)))
                               (length (attribute result)))
           #:with ids (filter identity (append (attribute arg.mandatory-id)
                                               (attribute arg.optional-id)
                                               (attribute arg.mandatory-kw-id)
                                               (attribute arg.optional-kw-id)
                                               (attribute result.id))) 
           #:attr specs (append (make-argument-specs mandatory-spec
                                                     (syntax->list #'ids)
                                                     (attribute arg.mandatory-id)
                                                     (attribute arg.mandatory-ctc))
                                (make-argument-specs optional-spec
                                                     (syntax->list #'ids)
                                                     (attribute arg.optional-id)
                                                     (attribute arg.optional-ctc))
                                (list (make-rest-spec (syntax->list #'ids)
                                                      (attribute rest.id)
                                                      (attribute rest.ctc)
                                                      (+ (length (filter identity (attribute arg.mandatory-id)))
                                                         (length (filter identity (attribute arg.optional-id))))))
                                (make-keyword-specs mandatory-keyword-spec
                                                    (syntax->list #'ids)
                                                    (attribute arg.mandatory-kw)
                                                    (attribute arg.mandatory-kw-id)
                                                    (attribute arg.mandatory-kw-ctc))
                                (make-keyword-specs optional-keyword-spec
                                                    (syntax->list #'ids)
                                                    (attribute arg.optional-kw)
                                                    (attribute arg.optional-kw-id)
                                                    (attribute arg.optional-kw-ctc))
                                (make-argument-specs result-spec
                                                     (syntax->list #'ids)
                                                     (attribute result.id)
                                                     (attribute result.ctc)))
           #:attr decl #`(#,@(apply append-syntax (attribute arg.decl)) . rest.id)
           #:attr invoc #`(#,@(apply append-syntax (attribute arg.invoc)) . rest.id)
           #:fail-when (duplicate-ids? (syntax->list #'ids))
           "duplicate identifiers in signature"
           #:fail-when (let ([result-ids (map spec-id (filter result-spec? (attribute specs)))]
                             [arg-uses   (map spec-uses (filter argument-spec? (attribute specs)))])
                         (ormap (lambda (use-set)
                                  (ormap (lambda (result-id)
                                           (member result-id use-set free-identifier=?))
                                         result-ids))
                                arg-uses))
           "argument contracts cannot refer to results"))

(define-splicing-syntax-class sig/defaults
  #:datum-literals (: ..)
  #:attributes (arity decl sig invoc)
  (pattern (~seq (arg:arg/defaults ...) (result:result ...))
           #:attr arity (arity (length (filter identity (attribute arg.mandatory-id)))
                               (length (filter identity (attribute arg.optional-id)))
                               (map syntax->datum (filter identity (attribute arg.mandatory-kw)))
                               (map syntax->datum (filter identity (attribute arg.optional-kw)))
                               (length (attribute result)))
           #:attr sig #`((#,@(apply append-syntax (attribute arg.sig))) (result ...))
           #:attr decl #`(#,@(apply append-syntax (attribute arg.decl)))
           #:attr invoc #`(#,@(apply append-syntax (attribute arg.invoc)))
           #:fail-when (duplicate-ids? (append (attribute arg.mandatory-id)
                                               (attribute arg.optional-id)
                                               (attribute arg.mandatory-kw-id)
                                               (attribute arg.optional-kw-id)
                                               (attribute result.id)))
           "duplicate identifiers in signature")
  (pattern (~seq (arg:arg/defaults ... .. rest:rest-arg) (result:result ...))
           #:attr arity (arity (length (filter identity (attribute arg.mandatory-id)))
                               #f
                               (map syntax->datum (filter identity (attribute arg.mandatory-kw)))
                               (map syntax->datum (filter identity (attribute arg.optional-kw)))
                               (length (attribute result)))
           #:attr sig #`((#,@(apply append-syntax (attribute arg.sig)) .. rest) (result ...))
           #:attr decl #`(#,@(apply append-syntax (attribute arg.decl)) . rest.id)
           #:attr invoc #`(#,@(apply append-syntax (attribute arg.invoc)) . rest.id)
           #:fail-when (duplicate-ids? (append (attribute arg.mandatory-id)
                                               (attribute arg.optional-id)
                                               (attribute arg.mandatory-kw-id)
                                               (attribute arg.optional-kw-id)
                                               (attribute result.id)
                                               (list (attribute rest.id))))
           "duplicate identifiers in signature"))
