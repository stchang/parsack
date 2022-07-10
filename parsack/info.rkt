#lang setup/infotab
(define version "0.5")
(define collection 'multi)
(define deps '("base" "parsack-lib" "parsack-doc" "parsack-test"))
(define implies '("parsack-lib" "parsack-doc" "parsack-test"))
(define build-deps '("rackunit-lib"
                     "scribble-lib"
                     "racket-doc"))
(define pkg-authors '(stchang))
(define name "parsack")
