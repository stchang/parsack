#lang scribble/manual
@(require scribble/eval
          scribble/base
         (for-label parsack
                     racket/contract/base
                     (rename-in racket/base [string mk-string])))

@title{Parsec implementation in Racket}

@(define the-eval (make-base-eval))
@(the-eval '(require parsack))

@defmodule[parsack]

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Parsec implementation in Racket. See @cite["parsec"].


@section{Parsers}

A @deftech{parser} is a function that consumes an @racket[input-port?] and returns
a parse result. A parse result is most commonly a @racket[char?] or a list of
chars, but is ultimately determined by each specific parser.

Two factors are considered when combining parsers:
@itemlist[
 @item{whether a parser consumes input, i.e., whether data was read from the input port,}
 @item{or whether a parser succeeds or fails.}]
These two factors are not mutually exclusive and some combinators only use one
of the factors while others use both to determine the parse result.

Specifically, @racket[Consumed] and @racket[Empty] struct results indicate
input-consumption and no-input-consumption, respectively, while the
@racket[Ok] and @racket[Error] structs indicate success and failure,
respectively.
See @secref{parse-structs} for more details about information contained in the
parse result. In general, users should use combinators below to compose
parsers and parse results, rather than directly handle the structs.

@;; ---------------------------------------------------------------------------
@section{Basic parsing functions}

@defproc[(parse-result [p parser?] [input (or/c string? path? input-port?)])
         any/c]{
  Parses input @racket[input] with parser @racket[p] and returns the parse
  result.

 The input can be either a string, a filepath, or an input port. A string or
 path input is converted to an input port before parsing begins.

 Raises the @racket[exn:fail:parsack] exception on parser failure.
 
@examples[#:eval the-eval
  (parse-result $letter "abc")
  (parse-result $letter "123")
  (parse-result (many $letter) "abc123")
  (parse-result (many $letter) "123")
  ]}

@defproc[(parse [p parser?] [input (or/c string? path? input-port?)])
         (or/c Consumed? Empty?)]{
  Parses input @racket[input] with parser @racket[p] and returns a
 @racket[Consumed] or @racket[Empty] struct that contains the result.

 The input can be either a string, a filepath, or an input port. A string or
 path input is converted to an input port before parsing begins.
                      
 Raises the @racket[exn:fail:parsack] exception on parser failure.
  
@examples[#:eval the-eval
  (parse $letter "abc")
  (parse $letter "123")
  (parse (many $letter) "abc123")
  (parse (many $letter) "123")
  ]}

@;; ---------------------------------------------------------------------------
@section{Basic parsing forms}

@defproc[(return [x any/c]) parser?]{
  Creates a parser that consumes no input and always succeeds, returning @racket[x].
  @examples[#:eval the-eval
    (parse (return "b") "a")]}

@defproc[(>>= [p parser?] [f (-> any/c parser?)]) parser?]{
  Monadic bind operator for parsers. Creates a parser that first parses with @racket[p].

 If @racket[p] fails, the error result is returned.

 Otherwise, the parse result is passed to @racket[f], and continues parsing with
 the parser created from applying @racket[f].
 
  @examples[#:eval the-eval
    (parse-result 
     (>>= (char #\() 
          (λ (skip1) 
            (>>= $letter 
                 (λ (x) 
                   (>>= (char #\)) 
                        (λ (skip2) 
                          (return x)))))))
     "(a)")]}

@defproc[(>> [p parser?] [q parser?]) parser?]{
  Equivalent to @racket[(>>= p (λ (x) q))] where @racket[x] is not in @racket[q].

 Creates a parser that first parses with @racket[p], and if successful,
 ignores the result, then parses with @racket[q].
  
  @examples[#:eval the-eval
    (parse-result 
     (>> (char #\() 
         (>>= $letter 
              (λ (x) 
                (>> (char #\)) 
                    (return x)))))
     "(a)")]}


@defform/subs[(parser-compose bind-or-skip ...+)
              ([bind-or-skip (x <- parser) parser]
               [parser parser?]
               [x identifier?])]{
  Composes parsers. Syntactic wrapper for a chain of @racket[>>=] operators.
                                                     
  @examples[#:eval the-eval
    (parse-result 
     (parser-compose (char #\[)
                     (x <- $letter)
                     (y <- $letter)
                     (char #\])
                     (return (list x y)))
     "[ab]")]}

@defform/subs[(parser-seq skippable ... maybe-combine)
              ([skippable (~ parser) parser]
               [parser parser?]
               [maybe-combine (code:line) (code:line #:combine-with combine)]
               [combine (-> any/c any/c)]
               )]{
  Combine parsers in "arrow" style instead of monadically. Applies all the given parsers, combines the results with the given @racket[combine] function, and then @racket[return]s it. The result of any parsers wrapped with @racket[~] is not given to @racket[combine].

  The default @racket[combine] function is @racket[list] so @racket[(parser-seq p q)] is syntactically equivalent to @racket[(parser-compose (x <- p) (y <- q) (return (list x y)))].
  
  Use @racket[parser-seq] instead of @racket[parser-compose] when you don't need the result of any parser other than to return it. Use @racket[parser-compose] when you need the result of one parse to build subsequent parsers, for example when parsing matching html open-close tags, or if you need to do additional processing on the parse result before returning.
  
  @examples[#:eval the-eval
    (parse-result 
     (parser-seq (~ (char #\[))
                 $letter 
                 $letter
                 (~ (char #\])))
     "[ab]")]}

@defform/subs[(parser-cons p q)
              ([p parser?] [q parser?])]{
  Syntactically equivalent to @racket[(parser-seq p q #:combine-with cons)]
                              
  @examples[#:eval the-eval
    (parse-result 
     (parser-cons $letter (many $letter))
     "abcde")]}

@defform/subs[(parser-one p ...)
              ([p (~> parser) parser]
               [parser parser?])]{
  Combines parsers but only return the result of the parser wrapped with @racket[~>]. Only one parser may be wrapped with @racket[~>].

  For example, @racket[(parser-one p1 (~> p2) p3)] is syntactically equivalent to @racket[(parser-seq (~ p1) p2 (~ p3) #:combine-with (λ (x) x))], which is equivalent to @racket[(parser-compose p1 (x <- p2) p3 (return x))].
  
  @examples[#:eval the-eval
    (parse-result
     (parser-one (char #\() (~> $letter) (char #\)))
     "(a)")]}

@;;----------------------------------------------------------------------------
@section{Basic Combinators}

@defproc[(<or> [p parser?] [q parser?] ...) parser?]{
  Creates a parser that tries the given parsers in order, returning with the result of the first parser that consumes input. Errors if not given at least one parser.

  @examples[#:eval the-eval
    (parse-result (<or> $letter $digit) "1")]

  NOTES:
  @itemize[
    @item{@racket[<or>] continues to try subsequent parsers so long as each of the previous parsers consumes no input, even if one of the previous parsers returns successfully. Thus @racket[<or>] implements "longest match" (see  @cite["parsec"] for more details).

  @examples[#:eval the-eval
    (parse-result (<or> (return null) $digit) "1")]}

    @item{See also @racket[<any>], a related parser that immediately returns when it encounters a successful parse, even if the parse consumed no input.

  @examples[#:eval the-eval
    (parse-result (<any> (return null) $digit) "1")]}
  
    @item{But if no parsers consume input, then @racket[<or>] backtracks to return the result of the first success.
  
  @examples[#:eval the-eval
    (parse-result (<or> (return "a") (return "b") (return "c")) "1")]}
  
    @item{If one of the given parsers consumes input, and then errors, @racket[<or>] returns immediately with the error.  
  @examples[#:eval the-eval
    (parse-result 
     (<or> (string "ab")
           (string "ac"))
     "ac")]
  Use @racket[try] to reset the input on a partial parse.
  @examples[#:eval the-eval
    (parse-result 
     (<or> (try (string "ab"))
           (string "ac"))
     "ac")]}
  ]}

@defproc[(choice [ps (listof parser?)]) parser?]{
  Same as @racket[(apply <or> ps)].}

@defproc[(<any> [p parser?] [q parser?] ...) parser?]{
  Creates a parser that tries the given parsers in order, returning with the result of the first successful parse, even if no input is consumed. Errors if not given at least one parser. See @racket[<or>] for a related, alternative parser.

  @examples[#:eval the-eval
    (parse-result (<any> $letter $digit) "1")]

  NOTES:
  @itemize[
    @item{@racket[<any>] immediately returns when it encounters a successful parse, even if the parse consumed no input.

  @examples[#:eval the-eval
    (parse-result (<any> (return null) $digit) "1")]}

  @item{See also @racket[<or>], a related parser that continues to try subsequent parsers so long as each of the previous parsers consumes no input, even if one of the previous parsers returns successfully.

  @examples[#:eval the-eval
    (parse-result (<or> (return null) $digit) "1")]}

]}

@defproc[(many [p parser?] 
               [#:till end parser? (return null)]
               [#:or orcomb (-> parser? ... (or/c Consumed? Empty?)) <or>])
         parser?]{
  Creates a parser that repeatedly parses with @racket[p] zero or more times.
                                               
  Stops when given @racket[end] parser parses "successfully" where "success" is determined by the given @racket[orcomb] combinator. By default @racket[orcomb] is @racket[<or>] and @racket[end] must consume input to be successful. If @racket[orcomb] is @racket[<any>], then @racket[many] terminates as soon as @racket[end] returns non-error.}
@defproc[(many1 [p parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] one or more times.}

@defproc[(manyTill [p parser?][end parser?] 
                   [#:or orcomb (-> parser? ... (or/c Consumed? Empty?)) <or>])
         parser?]{
  Creates a parser that repeatedly parses with @racket[p] zero or more times, where parser @racket[end] is tried after each @racket[p] and the parsing ends when @racket[end] succeeds.
                                               
  Equivalent to @racket[(many p #:till end #:or orcomb)].}
@defproc[(many1Till [p parser?][end parser?]
                    [#:or orcomb (-> parser? ... (or/c Consumed? Empty?)) <or>]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] one or more times, where parser @racket[end] is tried after each @racket[p] and the parsing ends when @racket[end] succeeds.}

@defproc[(manyUntil [p parser?][end parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] zero or more times, where parser @racket[end] is tried after each @racket[p] and the parsing ends when @racket[end] returns non-error, even if @racket[end] consumes no input.
                                               
  Equivalent to @racket[(manyTill p end #:or <any>)].}
@defproc[(many1Until [p parser?][end parser?]) parser?]{
  Creates a parser that repeatedly parses with @racket[p] one or more times, where parser @racket[end] is tried after each @racket[p] and the parsing ends when @racket[end] returns non-error, even if @racket[end] consumes no input.
                                               
  Equivalent to @racket[(many1Till p end #:or <any>)].}

@subsection{A Basic CSV parser}

Here is an implementation of a basic parser for comma-separated values (CSV).

A cell consists of any characters except a comma or newline.
@interaction[#:eval the-eval
  (define $oneCell (many (noneOf ",\n")))]

A series of cells are separated by commas. To parse cells, we use two mutually referential parsers.

@interaction[#:eval the-eval
  (define $cells (parser-cons $oneCell $remainingCells))
  (define $remainingCells (<or> (>> (char #\,) $cells)
                                (return null)))]

A line is a series of cells followed by a newline.
@interaction[#:eval the-eval
  (define $line (parser-one (~> $cells) $eol))]

A CSV string is a series of lines.
@interaction[#:eval the-eval
  (define $csv (many $line))
  (parse-result $csv "cell1,cell2\ncell3,cell4\n")]


@;; ---------------------------------------------------------------------------
@section{Other combinators}

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
@defproc[(between [open parser?][close parser?][p parser?]) parser?]{
  Creates a parser that parses with @racket[p] only if it's surround by @racket[open] and @racket[close]. Only the result of @racket[p] is returned.}
@defproc[(lookAhead [p parser?]) parser?]{
  Creates a parser that parses with @racket[p] and returns its result, but consumes no input.}
@defproc[(<!> [p parser?] [q parser? $anyChar]) parser?]{
  Similar to @racket[noneOf] but more general. Creates a parser that errors if @racket[p] successfully parses input, otherwise parses with @racket[q].
  @racket[q] defaults to @racket[$anyChar] if unspecified.}
@defproc[(notFollowedBy [p parser?]) parser?]{
  Creates a parser that succeeds and return nothing if @racket[p] fails, and fails if @racket[p] succeeds. Does not consume input.}

@;; ---------------------------------------------------------------------------
@section{Character parsing}

@defproc[(satisfy [p? (-> any/c boolean?)]) parser?]{
  Creates a parser that consumes and returns one character if it satisfies predicate @racket[p?].}
@defproc[(char [c char?]) parser?]{
  Creates a parser that parses and returns char @racket[c], case-sensitive.}
@defproc[(charAnyCase [c char?]) parser?]{
  Creates a parser that parses and returns char @racket[c], case-insensitive.}
@defproc[(noneOf [str string?]) parser?]{
  Creates a parser that consumes and returns one character if the character does not appear in @racket[str].}
@defproc[(oneOf [str string?]) parser?]{
  Creates a parser that consumes and returns one character if the character appears in @racket[str].}
@defproc[(oneOfStrings [str string?] ...) parser?]{
  Creates a parser that consumes and returns any of the @racket[str]s, case-sensitive. Note that the parse result is @racket[(listof char?)] not @racket[string?].}
@defproc[(oneOfStringsAnyCase [str string?] ...) parser?]{
  Creates a parser that consumes and returns any of the @racket[str]s, case-insensitive. Note that the parse result is @racket[(listof char?)] not @racket[string?].}
@defproc[(string [str string?]) parser?]{
  Creates a parser that parses and returns @racket[str] as a list of chars.}

@section{Constant parsers}
This library uses the $ prefix for identifiers that represent parsers (as opposed to combinators).
@defthing[$letter parser?]{Parses an alphabetic char.}
@defthing[$digit parser?]{Parses an numeric char.}
@defthing[$alphaNum parser?]{Parses a letter or digit.}
@defthing[$hexDigit parser?]{Parses a hex char.}
@defthing[$space parser?]{Parses a space.}
@defthing[$spaces parser?]{Parses zero or more spaces.}
@defthing[$anyChar parser?]{Parses any char.}
@defthing[$newline parser?]{Parses newline char. This is the singular @racket[#\n] character. See also @racket[$eol].}
@defthing[$tab parser?]{Parses tab char.}
@defthing[$eol parser?]{Parses end of line. @racket[$eol] succeeds on @racket["\n"], @racket["\r"], @racket["\r\n"], or @racket["\n\r"]. 
                        See also @racket[$newline].}
@defthing[$eof parser?]{Parses end of file.}
@defthing[$identifier parser?]{Parsers string containing only numbers, letters, or underscore.}

@;; ---------------------------------------------------------------------------
@section[#:tag "userstate"]{User State}

@defproc[(setState [key symbol?] [val any/c]) parser?]{
  Creates a parser that sets user state and whose result is the prior value of @racket[key] (or @racket[#f] is no value was set).}

@defproc[(getState [key symbol?]) parser?]{
  Creates a parser whose result is the value for the state of @racket[key] (or @racket[#f] is there no value was set.)}

@defform[(withState ([key value] ...) parser)]{
  A form that creates a parser analog of @racket[parameterize], by using @racket[parser-compose] and @racket[setState]. The original value of each @racket[key] is saved, the new value is set during the evaluation of @racket[parser], then the original value is restored.}

@;; ---------------------------------------------------------------------------
@section{Error handling combinators}

@defproc[(try [p parser?]) parser?]{
  Lookahead function. Creates a parser that tries to parse with @racket[p]
 but does not consume input if @racket[p] fails.
  @examples[#:eval the-eval
  ((string "ab") (open-input-string "ac"))
  ((try (string "ab")) (open-input-string "ac"))
  ]}

@defproc[(<?> [p parser?] [expected string?]) parser?]{
  Creates a parser that tries to parser with @racket[p] and returns error msg @racket[expected] if it fails.}

@defthing[$err parser?]{Parser that always returns error.}

@defproc[(err [expected string?]) parser?]{ 
  Like @racket[$err] but allows custom expected msg.}

@section{Bytestring parsing}

@defproc[(byte [b byte?]) parser?]{
  Creates a parser that parses and returns byte @racket[b].}
@defproc[(bytestring [bstr bytes?]) parser?]{
  Creates a parser that parses and returns @racket[bstr] as a list of bytes.}

@;; ---------------------------------------------------------------------------
@section[#:tag "parse-structs"]{Parse Result Structs}

A @tech{parser} is a function that consumes an @racket[input-port?] and
returns either a @racket[Consumed], or an @racket[Empty] struct.

In general, users should use the above combinators to connect parsers and parse results, rather than manipulate these structs directly.

@defstruct*[Consumed ([reply (or/c Ok? Error?)])]{
  This is the result of parsing if some input is consumed. 
  
  @examples[#:eval the-eval
  (parse $letter "abc")
  (parse (many $letter) "abc123")
  ((string "ab") (open-input-string "ac"))
  ]}

@defstruct*[Empty ([reply (or/c Ok? Error?)])]{
  This is the result of parsing if no input is consumed.
  @examples[#:eval the-eval
  (parse (many $letter) "123")
  ]}

@defstruct*[Ok ([parsed any/c])]{
  Contains the result of parsing @racket[parser].
  
  The parse result can be any value and depends on the specific parser that produces the this struct.
  
@examples[#:eval the-eval
  (parse $letter "abc")
  (parse (many $letter) "123")
  ]}

@defstruct*[Error ()]{
  Indicates parse error.
 
  @examples[#:eval the-eval
  ($letter (open-input-string "123"))
  ]}

@defstruct*[(exn:fail:parsack exn:fail) ()]{
@racket[parse-result] and @racket[parse] throw this exception on parse failure.}
                                
@(bibliography
  (bib-entry #:key "parsec"
             #:author "Daan Leijen and Erik Meijer"
             #:title "Parsec: A practical parser library"
             #:location "Electronic Notes in Theoretical Computer Science"
             #:date "2001"))
