#lang info

(define collection "guard")

(define deps
  '("base"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))

(define scribblings '(("scribblings/guard.scrbl" ())))

(define pkg-desc "Guard statements for Racket")

(define version "0.0")

(define pkg-authors '(jackfirth))

(define license '(Apache-2.0 OR MIT))
