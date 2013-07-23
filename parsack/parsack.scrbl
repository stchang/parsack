#lang scribble/manual
@(require scribble/eval
          (for-label "parsack.rkt"
                     (rename-in racket/base [string mk-string])))

@title{Parsec implementation in Racket}

@(define the-eval (make-base-eval))
@(the-eval '(require "parsack.rkt"))

@defmodule[parsack]

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Parsec implementation in Racket.