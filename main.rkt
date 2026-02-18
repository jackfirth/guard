#lang racket/base


(provide define/guard
         guard
         guard-match
         guarded-block)


(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse/lib/function-header)
         racket/match
         syntax/parse/define)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-syntax-parse-rule (guard condition:expr #:else fail-body:expr ...+)
  #:do [(raise-syntax-error #false "must be used immediately within a guarded block" this-syntax)]
  (void))


(define-syntax-parse-rule (guarded-block form:expr ...)
  (let () (guarded-begin form ...)))


(define-syntax (guarded-begin stx)
  (syntax-parse stx
    #:track-literals
    [(_) #'(begin)]
    [(_ initial-form leftover-form ...)
     (define expanded-initial-form
       (local-expand
        #'initial-form (syntax-local-context) (list #'guard #'define-values)))
     (syntax-parse (syntax-disarm expanded-initial-form #false)
       #:literal-sets (kernel-literals)
       #:literals (guard)
       #:track-literals
       [(begin ~! subform:expr ...)
        #'(guarded-begin subform ... leftover-form ...)]
       [(define-values ~! . _)
        #`(begin #,expanded-initial-form (guarded-begin leftover-form ...))]
       [(define-syntaxes ~! . _)
        #`(begin #,expanded-initial-form (guarded-begin leftover-form ...))]
       [(guard condition:expr #:else ~! else-form:expr ...+)
        #'(cond
            [condition (guarded-begin leftover-form ...)]
            [else (guarded-begin else-form ...)])]
       [e:expr #'(begin e (guarded-begin leftover-form ...))])]))


(define-syntax-parse-rule (define/guard header:function-header body:expr ...+)
  (define header (guarded-begin body ...)))


(begin-for-syntax
  (define-syntax-class guard-pattern
    #:attributes ([subject-id 1] match-test definition)
    #:literals (values)

    (pattern (values pattern:expr ...)
      #:with (subject-id ...) (generate-temporaries #'(pattern ...))
      #:with (placeholder ...) (make-list (length (attribute pattern)) #'_)
      #:with match-test #`(match* (subject-id ...) [(pattern ...) #true] [(placeholder ...) #false])
      #:with definition #'(match-define-values (pattern ...) (values subject-id ...)))

    (pattern pattern:expr
      #:with only-subject-id (generate-temporary)
      #:with (subject-id ...) (list #'only-subject-id)
      #:with match-test #'(match only-subject-id [pattern #true] [_ #false])
      #:with definition #'(match-define pattern only-subject-id))))


(define-syntax-parse-rule (guard-match pattern:guard-pattern subject-expr:expr
                            #:else failure-body ...+)
  (begin
    (define-values (pattern.subject-id ...) subject-expr)
    (define subject-matched? pattern.match-test)
    (guard subject-matched? #:else failure-body ...)
    pattern.definition))


(module+ test
  (test-case "guard-match"

    (test-case "single value"
      (define/guard (f opt)
        (guard-match (? number? x) opt #:else "failed")
        (format "x = ~a" x))
      (check-equal? (f "not a number") "failed")
      (check-equal? (f 5) "x = 5"))

    (test-case "multiple values"
      (define/guard (f opt1 opt2)
        (guard-match (values (? number? x) (? number? y)) (values opt1 opt2) #:else "failed")
        (format "x = ~a, y = ~a" x y))
      (check-equal? (f "not a number" 7) "failed")
      (check-equal? (f 5 "not a number") "failed")
      (check-equal? (f 5 7) "x = 5, y = 7"))))
