#lang racket/base


(provide define/guard
         guard
         guard-match
         guarded-block)


(require (for-syntax racket/base
                     syntax/parse/lib/function-header)
         racket/match
         syntax/parse/define)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-syntax (guard stx)
  (raise-syntax-error
   #false "must be used immediately within a guarded block" stx))


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
     (syntax-protect
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
        [e:expr #'(begin e (guarded-begin leftover-form ...))]))]))


(define-syntax-parse-rule (define/guard header:function-header body:expr ...+)
  (define header (guarded-begin body ...)))


(define-syntax-parse-rule (guard-match pattern subject:expr #:else failure-body ...+)
  (begin
    (define subject-matched? (match subject [pattern #true] [_ #false]))
    (guard subject-matched? #:else failure-body ...)
    (match-define pattern subject)))


(module+ test
  (test-case "guard-match"
    (define/guard (f opt)
      (guard-match (? number? x) opt #:else "failed")
      (format "x = ~a" x))
    (check-equal? (f "not a number") "failed")
    (check-equal? (f 5) "x = 5")))
