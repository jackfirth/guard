#lang scribble/manual


@(require (for-label guard
                     racket/base
                     racket/list
                     racket/match)
          (submod guard/private/scribble-evaluator-factory doc)
          scribble/example)


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'guard)
    #:private (list 'racket/base)))


@title{Guard Statements}
@author{jackfirth}


@defmodule[guard]


This package provides a Racket syntax for @deftech{guard statements}. A guard statement ensures that
condition is true before executing statements after the guard. Boolean conditions can be checked with
@racket[guard], and pattern matching conditions can be checked with @racket[guard-match]. Guard
statements can only be used within @deftech{guarded blocks}, which are either uses of the
@racket[guarded-block] macro or bodies of functions defined with @racket[define/guard].

Guard statements allow writing code linearly when it would ordinarily require deep nesting of forms
like @racket[cond] and @racket[match]. Compare the following two function definitions, one of which
uses traditional Racket forms and the other of which uses guard statements.

@(examples
  #:eval (make-evaluator)
  #:once
  #:no-prompt
  #:label "Traditional approach:"
  (define (lists-equal? xs ys)
    (cond
      [(empty? xs) (empty? ys)]
      [(empty? ys) #false]
      [else
       (match-define (cons x rest-xs) xs)
       (match-define (cons y rest-ys) ys)
       (and (equal? x y) (lists-equal? rest-xs rest-ys))])))

@(examples
  #:eval (make-evaluator)
  #:once
  #:no-prompt
  #:label "Using guard statements:"
  (define/guard (lists-equal? xs ys)
    (guard-match (cons x rest-xs) xs #:else
      (empty? ys))
    (guard-match (cons y rest-ys) ys #:else
      #false)
    (and (equal? x y) (lists-equal? rest-xs rest-ys))))


Guard statements cooperate with macro expansion. Macros can expand into uses of @racket[guard] and
@racket[guard-match]. Such user-defined guard statements are recognized by @racket[guarded-block] and
@racket[define/guard] via local expansion.


@defform[(guard condition-expr #:else fail-body ...+)
         #:contracts ([condition-expr any/c])]{

 A @tech{guard statement} that ensures that @racket[condition-expr] evaluates to a non-false value. If
 it's false, the surrounding @tech{guarded block} short circuits evaluation with the result of
 executing @racket[fail-body ...].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define/guard (add-positive x y)
      (guard (positive? x) #:else
        'nonpositive-x)
      (guard (positive? y) #:else
        'nonpositive-y)
      (+ x y)))

   (add-positive -4 7)
   (add-positive 4 -7)
   (add-positive 4 7))}


@defform[(guard-match match-pattern expr #:else fail-body ...+)
         #:contracts ([expr any/c])]{

 A @tech{guard statement} that evaluates @racket[expr] and ensures that its result matches
 @racket[match-pattern]. If the result does not match the given pattern, the surrounding
 @tech{guarded block} short circuits evaluation with the result of executing @racket[fail-body ...].
 Upon successful matching, the bindings in @racket[match-pattern] are made available to the
 surrounding definition context. This form is similar to @racket[match-define], except that the
 @racket[fail-body ...] branch is taken if the match fails.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define/guard (zip-lists zipper xs ys)
      (guard-match (cons x rest-xs) xs #:else
        '())
      (guard-match (cons y rest-ys) ys #:else
        '())
      (cons (zipper x y) (zip-lists zipper rest-xs rest-ys))))

   (zip-lists (λ (action animal) (format "~a, ~a!" action animal))
              (list "jump" "dig" "hop")
              (list "dog" "mole" "rabbit")))}


@defform[(define/guard (head args) body ...+)
         #:grammar ([head id
                     (head args)]
                    [args (code:line arg ...)
                     (code:line arg ... @#,racketparenfont{.} rest-id)]
                    [arg arg-id
                     [arg-id default-expr]
                     (code:line keyword arg-id)
                     (code:line keyword [arg-id default-expr])])]{

 Defines a function like @racket[define], but where the function body is wrapped in
 @racket[guarded-block].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define/guard (filter-and-double-numbers xs)
      (guard-match (cons x rest-xs) xs #:else
        '())
      (guard (number? x) #:else
        (filter-and-double-numbers rest-xs))
      (cons (* x 2) (filter-and-double-numbers rest-xs))))

   (filter-and-double-numbers (list 1 2 'apple 3 'banana 4 5)))}


@defform[(guarded-block body ...+)]{

 Evaluates each @racket[body], except that any @tech{guard statements} are treated specially. When a
 guard statement fails, it short circuits the evaluation of the entire enclosing
 @racket[guarded-block].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define (double-numbers-only xs)
      (for/list ([x (in-list xs)])
        (guarded-block
         (guard (number? x) #:else
           x)
         (* x 2)))))

   (double-numbers-only (list 1 2 'apple 3 'banana 4 5)))}
