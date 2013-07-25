#lang scribble/manual
@(require scribble/eval
          (for-label "parsack.rkt"
                     (rename-in racket/base [string mk-string])))

@title{Parsec implementation in Racket}

@(define the-eval (make-base-eval))
@(the-eval '(require "parsack.rkt"))

@defmodule[parsack]

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Parsec implementation in Racket. See @cite["parsec"].

@;; ---------------------------------------------------------------------------
@section{Basic parsing functions}

@defproc[(parse [p parser?] [input string?]) (or/c Consumed? Empty?)]{
  Parses input string @racket[input] with parser @racket[p] and returns the result}

@;; ---------------------------------------------------------------------------
@section{Basic parsing combinators/forms}

@defform/subs[(parser-compose bind-or-skip ...)
              ([bind-or-skip parser (x <- parser)]
               [parser parser?]
               [x identifier?])]{
  Composes parsers. Syntactic wrapper for @racket[>>=] operator.}

@defproc[(>>= [p parser?] [f (-> any/c parser?)]) parser?]{
  Monadic bind operator. Creates a parser that first parses with @racket[p], passes the result to @racket[f], then parses with the result of applying @racket[f]. If @racket[p] succeeds and consumes input, the application of @racket[f] is delayed. This avoids space leaks (see @cite["parsec"]).}

@defproc[(>> [p parser?] [q parser?]) parser?]{
  Creates a parser that first parses with @racket[p], ignores the result, then parses with @racket[q]. If @racket[p] consumes input, then parsing with @racket[q] is delayed. Equivalent to @racket[(>>= p (λ (x) q))] where @racket[x] is not bound in @racket[q].}

@defproc[(return [x any/c]) parser?]{
  Creates a parser that consumes no input and returns @racket[x].}

@defproc[(<or> [p parser?] ...) parser?]{
  Creates a parser that tries the given parses in order, returning with the first successful result.}
@defproc[(choice [p parser?] ...) parser?]{
  Same as @racket[<or>].}

@;; ---------------------------------------------------------------------------
@section{Other combinators}

@defproc[(many [p parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] zero or more times.}
@defproc[(many1 [p parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] one or more times.}
@defproc[(skipMany [p parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] zero or more times, but does not return the result.}
@defproc[(skipMany1 [p parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] one or more times, but does not return the result.}
@defproc[(sepBy [p parser?][sep parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] zero or more times, where each parse of @racket[p] is separated with a parse of @racket[sep]. Only the results of @racket[p] are returned.}
@defproc[(sepBy1 [p parser?][sep parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] one or more times, where each parse of @racket[p] is separated with a parse of @racket[sep]. Only the results of @racket[p] are returned.}
@defproc[(endBy [p parser?][end parser?]) parser?]{
  Like @racket[sepBy] except for an extra @racket[end] at the end.}
@defproc[(manyTill [p parser?][end parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] zero or more times, where parser @racket[end] is tried after each @racket[p] and the parsing ends when @racket[end] succeeds.}
@defproc[(between [open parser?][close parser?][p parser?]) parser?]{
  Creates a parser that parses with @racket[p] only if it's surround by @racket[open] and @racket[close]. Only the result of @racket[p] is returned.}

@;; ---------------------------------------------------------------------------
@section{Character parsing}

@defproc[(satisfy [p? (-> any/c boolean?)]) parser?]{
  Creates a parser that consumes and returns one character if it satisfies predicate @racket[p?].}
@defproc[(char [c char?]) parser?]{
  Creates a parser that parses char @racket[c].}
@defproc[(noneOf [str string?]) parser?]{
  Creates a parser that consumes and returns one character if the character does not appear in @racket[str].}
@defproc[(oneOf [str string?]) parser?]{
  Creates a parser that consumes and returns one character if the character appears in @racket[str].}
@defproc[(string [str string?]) parser?]{
  Creates a parser that parses but does not return @racket[str].}

@section{Constant parsers}
This library uses the $ prefix for identifiers that represent parsers (as opposed to combinators).
@defthing[$letter parser?]{Parses an alphabetic char.}
@defthing[$digit parser?]{Parses an numeric char.}
@defthing[$hexDigit parser?]{Parses a hex char.}
@defthing[$space parser?]{Parses a space.}
@defthing[$spaces parser?]{Parses zero or more spaces.}
@defthing[$anyChar parser?]{Parses any char.}
@defthing[$newline parser?]{Parses newline char.}
@defthing[$tab parser?]{Parses tab char.}
@defthing[$eol parser?]{Parses end of line.}
@defthing[$eof parser?]{Parses end of file.}
@defthing[$identifier parser?]{Parsers string containing only numbers, letters, or underscore.}
@defthing[$err parser?]{Parser that always returns error.}

@;; ---------------------------------------------------------------------------
@section{Error handling combinators}

@defproc[(try [p parser?]) parser?]{
  Lookahead function. Creates a parser that tries to parse with @racket[p] but does not consume input if @racket[p] fails.}

@defproc[(<?> [p parser?] [expected string?]) parser?]{
  Creates a parser that tries to parser with @racket[p] and returns error msg @racket[expected] if it fails.}

@;; ---------------------------------------------------------------------------
@section{Structs for Parsing}

A @deftech{parser} is a function that consumes a @racket[State] and returns either an error, or a @racket[Consumed], or an @racket[Empty].

@defstruct*[State ([str string?] [pos number?])]{
  Input to a parser. Consists of an input string and a position.}

@defstruct*[Consumed ([reply (or/c Ok? Error?)])]{
  This is the result of parsing if some input is consumed.}

@defstruct*[Empty ([reply (or/c Ok? Error?)])]{
  This is the result of parsing if no input is consumed.}

@defstruct*[Ok ([parsed any/c][rest State?][msg Msg?])]{
  Contains the result of parsing, remaining input, and any error messages.}

@defstruct*[Error ([msg Msg?])]{
  Indicates parse error.}
                                                        
@(bibliography
  (bib-entry #:key "parsec"
             #:author "Daan Leijen and Erik Meijer"
             #:title "Parsec: A practical parser library"
             #:location "Electronic Notes in Theoretical Computer Science"
             #:date "2001"))