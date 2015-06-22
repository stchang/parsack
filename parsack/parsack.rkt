#lang racket
(require "string-helpers.rkt")
(require (for-syntax syntax/parse racket/syntax))
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;; Parsec (with error messages) ;;;;;;;;;;;;;;;;;;;;;;;;;
;; implements monadic combinators for LL parsing

;; p is an InputPort when (input-port? p)

;; A [Maybe X] is an X or #f

;; A [Parser X] is a function: InputPort -> [Maybe X]

;; Naming conventions:
;; Parsers use a $-prefixed name.
;; Parser combinators have camelCase names, with 1st char lowercase

;; [Thunk String]
(define current-unexpected (make-parameter ""))
;; [List [Thunk String]]
(define current-expected (make-parameter null))
;; HashEq
(define user-state (make-parameter (hasheq)))

;; OLD: -----------------------------------------------------------------------
;; A [Parser X] is a function: State -> [Consumed X]
;;   where X = the type of the parsed output

;; A State is a (State String Pos UserState)
;; - parsers consume a State
(struct State (str pos user) #:transparent)
;; where str = input
;;       pos = Pos
;;       user = UserState

;; A Message is a (Msg Pos (-> String) [List (-> String)])
(struct Msg (pos str strs) #:transparent)
;; where pos = Pos
;;       str = (thunk of) unexpected input
;;       strs = (thunks of) possible expected inputs

;; A [Consumed X] is one of:
;; - (Consumed [Reply X])
;; - (Empty [Reply X])
(struct Consumed (reply) #:transparent) ;; should be lazy? (see >>= def)
(struct Empty (reply) #:transparent)

(define-match-expander Consumed! (syntax-rules () [(_ e) (Consumed (app force e))]))

;; A [Reply X] is one of:
;; - (Ok X State Message)
;; - (Error Message)
(struct Ok (parsed rest msg) #:transparent)
(struct Error (msg) #:transparent)

(define (err expected)
  (λ (in)
    (current-unexpected (thunk (port->string in)))
    (current-expected (list expected))
    #f))
(define $err
  (err "")
  #;(λ (in)
    (current-unexpected (thunk (port->string in)))
    (current-expected null)
    #f)
  #;(match-lambda [(State inp pos _) (Empty (Error (Msg pos inp null)))]))

(struct exn:fail:parsack exn:fail ())

(define-syntax-rule (parsack-error msg)
  (raise (exn:fail:parsack (string-append "parse ERROR: " msg)
                           (current-continuation-marks))))

;; A Pos is a (Pos line col ofs)
;; - numbers are 1-based
(struct Pos (line col ofs) #:transparent)
(define parse-source (make-parameter #f)) ;; not in Pos for efficiency
(define (start-pos)
  (Pos 1 1 1))
(define (incr-pos p c)
  (match* (p c)
    [((Pos line col ofs) #\newline) (Pos (add1 line) 1 (add1 ofs) )]
    [((Pos line col ofs) _        ) (Pos line (add1 col) (add1 ofs) )]))
(define (format-pos p)
  (match* (p (parse-source))
    [((Pos line col ofs) (? path-string? src))
     (format "~a:~a:~a:~a" src line col ofs)]
    [((Pos line col ofs) _)
     (format "~a:~a:~a" line col ofs)]))

;; creates a Parser that consumes no input and returns x
(define (return x)
  (λ (in)
    x)
  #;(match-lambda 
    [(and state (State _ pos _))
     (Empty (Ok x state (Msg pos "" null)))]))

;; creates a parser that consumes 1 char if it satisfies predicate p?
(define (satisfy p?)
  (λ (in)
    (define c (peek-char in))
    (cond
      [(eof-object? c)
       (current-unexpected "end of input")
       (current-expected null)
       #f]
      [(p? c)
       (current-unexpected "")
       (current-expected null)
       (read-char in)]
      [else
       (current-unexpected (mk-string c))
       (current-expected null)
       #f]))
  #;(match-lambda 
   [(State input pos user)
    (if (str-empty? input)
        (Empty (Error (Msg pos "end of input" null)))
        (let ([c (str-fst input)])
          (if (p? c)
              (let* ([new-pos (incr-pos pos c)]
                     [new-state (State (str-rst input) new-pos user)])
                (Consumed (Ok c new-state (Msg new-pos "" null))))
              (Empty (Error (Msg pos (mk-string c) null))))))]))

(define (ofString ? err)
  (λ (in)
    (define x/#f ((satisfy ?) in))
    (unless x/#f (current-expected (cons err (current-expected))))
    x/#f)
  #;(λ (state)
     (match ((satisfy ?) state)
       [(Consumed! (Error (Msg pos inp exp)))
        (Consumed (Error (Msg pos inp (cons err exp))))]
       [(Empty (Error (Msg pos inp exp)))
        (Empty (Error (Msg pos inp (cons err exp))))]
       [ok ok])))

(define (oneOf str)
  (ofString (make-char-in-string? str)
            (thunk (string-append "one of: " (str->strs str)))))

(define (noneOf str)
  (ofString (negate (make-char-in-string? str))
            (thunk (string-append "none of: " (str->strs str)))))
       
(define (make-char-in-string? str)
  ;; `(for/or ([c (in-string str)]))` is slow. A precomputed `seteqv`
  ;; surprisingly isn't that much better. However a precomputed
  ;; `hasheqv` IS significantly faster.
  (let ([ht (for/hasheqv ([c (in-string str)])
              (values c #t))])
    (lambda (c)
      (hash-ref ht c #f))))

(define (str->strs str)
  (format-exp (map mk-string (string->list str))))

(define (oneOfStrings . ss)
  (<?> (choice (map (compose1 try string) ss))
       (string-append "one of: "
                      (string-join (map ~s ss) ", "))))

(define (oneOfStringsAnyCase . ss)
  (<?> (choice (map (compose1 try stringAnyCase) ss))
       (string-append "one of: "
                      (string-join (map ~s ss) ", ")
                      " (case insensitive)")))

;; >= : [Parser X] (X -> [Parser Y]) -> [Parser Y]
;; Creates a Parser from a Parser p and a continuation f for p's result.
;; - if p succeeds but does not consume input, then f determines result
;; - if p succeeds and consumes input, return Consumed with thunk that delays
;;   parsing from f
;; - if p returns #f, do not continue with f
(define (>>= p f)
  (λ (in)
    (define pos (file-position in))
    (define p-result/#f (p in))
    (if p-result/#f
        (let ([saved-expected (current-expected)])
          (if (= pos (file-position in)) ; p consumed no input
              (let ([f-result/#f ((f p-result/#f) in)])
                (when (= pos (file-position in)) ; f consumed no input
                  (current-expected (append saved-expected (current-expected))))
                f-result/#f)
              (let ([f-result/#f ((f p-result/#f) in)])
                (when (and (= pos (file-position in)) (not f-result/#f))
                  (current-expected (append saved-expected (current-expected))))
                f-result/#f)))
        #f))
  #;(λ (input)
    (match (p input)
      [(Empty reply)
       (match reply
         [(Ok x rest msg1)
          (match ((f x) rest)
            [(Empty (Error msg2)) (mergeError msg1 msg2)]
            [(Empty (Ok x inp msg2)) (mergeOk x inp msg1 msg2)]
            [consumed consumed])]
         [err (Empty err)])]
      [(Consumed reply1)
       (Consumed ; arg to Consumed constructor should be delayed
        (lazy
         (match (force reply1)
           [(Ok x rest msg1)
            (match ((f x) rest)
              [(Consumed reply2) reply2]
              [(Empty (Error msg2)) (Error (merge msg1 msg2))]
              [(Empty ok) ok])]
           [error error])))])))
(define (>> p q) (>>= p (λ _ q)))

;; <|> choice combinator
;; first tries to parse with p, only tries q if p does not consume input
;; thus, <or> implements "longest match"
(define (<or>2 p q)
  (λ (in)
    (define pos (file-position in))
    (define p-result/#f (p in))
    (if (= (file-position in) pos)
        (let ([saved-expected (current-expected)]
              [q-result/#f (q in)])
          (if (= (file-position in) pos)
              (begin
                (current-expected (append saved-expected (current-expected)))
                (if p-result/#f p-result/#f q-result/#f))
              q-result/#f))
        p-result/#f))
  #;(λ (state)
    (match (p state)
      [(Empty (Error msg1))
       (match (q state)
         [(Empty (Error msg2)) (mergeError msg1 msg2)]
         [(Empty (Ok x inp msg2)) (mergeOk x inp msg1 msg2)]
         [consumed consumed])]
      [(Empty (Ok x inp msg1))
       (match (q state)
         [(Empty (Error msg2)) (mergeOk x inp msg1 msg2)]
         [(Empty (Ok _ _ msg2)) (mergeOk x inp msg1 msg2)]
         [consumed consumed])]
      [consumed consumed])))
(define (mergeConsumed x inp msg1 msg2) (Consumed (Ok x inp (merge msg1 msg2))))
(define (mergeOk x inp msg1 msg2) (Empty (Ok x inp (merge msg1 msg2))))
(define (mergeError msg1 msg2) (Empty (Error (merge msg1 msg2))))
(define/match (merge msg1 msg2)
  [((Msg _ _ exp1) (Msg pos inp exp2))
   (Msg pos inp (append exp1 exp2))])
                      
;; assumes (length args) >= 1
(define (<or> . args)
  (foldl (λ (p acc) (<or>2 acc p)) (car args) (cdr args)))

;; short-circuiting choice combinator
;; only tries 2nd parser q if p errors and consumes no input
;; differs from <or> in the case where p returns (Empty (Ok ...))
;; - <or>: parse with q
;; - <any>: stops
(define (<any>2 p q)
  (λ (in)
    (define pos (file-position in))
    (define result/#f (p in))
    (if (and (not result/#f) (= pos (file-position in)))
        (let ([pos (file-position in)]
              [saved-expected (current-expected)]
              [q-result/#f (q in)])
          (when (= pos (file-position in))
            (current-expected (append saved-expected (current-expected))))
          q-result/#f)
        result/#f))
  #;(λ (state)
    (match (p state)
      [(Empty (Error msg1))
       (match (q state)
         [(Empty (Error msg2)) (mergeError msg1 msg2)]
         [(Empty (Ok x inp msg2)) (mergeOk x inp msg1 msg2)]
         [consumed consumed])]
      [result result])))
                      
;; assumes (length args) >= 2
(define (<any> . args)
  (foldl (λ (p acc) (<any>2 acc p)) (car args) (cdr args)))


(define (option x p) (<or> p (return x)))
(define (optionMaybe p) (option #f p))
(define (optional p) 
  (<or> (>> p (return null))
        (return null)))

;; tries to parse with p but backtracks and does not consume input if error
(define (try p)
  (λ (in)
    (define in/peek (peeking-input-port in))
    (define result/#f (p in/peek))
    (if result/#f
        (begin
          (read-bytes (file-position in/peek) in)
          result/#f)
        #f))
  #;(λ (state)
    (match (p state)
      [(Consumed! (Error msg)) (Empty (Error msg))]
      [other other])))

;; Parse p and return the result, but don't consume input.
(define (lookAhead p)
  (λ (in)
    (p (peeking-input-port in)))
  #;(match-lambda
    [(and input (State inp pos _))
     (match (p input)
       [(Consumed! (Ok result _ (Msg _ str strs)))
        (Empty (Ok result input (Msg pos inp strs)))]
       [emp emp])]))

;; converts intermediate parse result to string -- for err purposes
;; Note: Efficiency of this matters, do dont call until throwing the exception
(define (result->str res)
  (cond [(char? res) (mk-string res)]
        [(and (list? res) (andmap char? res)) (list->string res)]
        [else res]))

(define (<!> p [q $anyChar])
  (λ (in)
    (define in/peek (peeking-input-port in))
    (define result/#f (p in/peek))
    (cond
      [(and result/#f (not (zero? (file-position in/peek))))
       (current-unexpected (λ () (result->str result/#f)))
       (current-expected `(,(λ () (format "not: ~a" (result->str result/#f)))))
       #f]
      [else (q in)]))
  #;(match-lambda 
    [(and state (State inp pos _))
     (match (p state)
       [(Consumed! (Ok res _ _))
        (Empty (Error (Msg pos (thunk (result->str res)) 
                           (list (thunk (format "not: ~a" (result->str res)))))))]
       [_ (q state)])]))

;; succeeds when p fails; does not consume input
(define (notFollowedBy p)
  (λ (in)
    (define result/#f (p (peeking-input-port in)))
    (cond
      [result/#f
       (current-unexpected (thunk (result->str result/#f)))
       (current-expected (list (thunk (format "not: ~a" (result->str result/#f)))))
       #f]
      [else
       (current-unexpected "")
       (current-expected null)
       null]))
  #;(match-lambda
    [(and state (State inp pos _))
     (match (p state)
       [(Consumed! (Ok res _ _))
        (Empty (Error (Msg pos (thunk (result->str res)) 
                           (list (thunk (format "not: ~a" (result->str res)))))))]
       [_ (Empty (Ok null state (Msg pos null null)))])]))

;; parse with p 0 or more times
;; some notes:
;; - default #:till can be (return <anything>), just needs to not consume input
;; - using many with #:or <any> and the default #:till will immediately return
;;   empty result without consuming input
(define (many p #:till [end (return 0)] #:or [<or> <or>]) 
  (<or> (>> end (return null))
        (parser-cons p (many p #:till end #:or <or>))))

;; parse with p 1 or more times
(define (many1 p #:till [end (return null)] #:or [<or> <or>])
  (parser-cons p (many p #:till end #:or <or>)))

(define (skipMany p) 
  (<or> (parser-compose p (skipMany p))
        (return null)))
(define (skipMany1 p) (parser-compose p (skipMany p)))

;; applies parser p zero or more times until parser end succeeds
(define (manyTill p end #:or [<or> <or>])
  (many p #:till end #:or <or>))

;; applies parser p one or more times until parser end succeeds
(define (many1Till p end #:or [<or> <or>])
  (parser-cons p (manyTill p end #:or <or>)))

;; manyUntil = manyTill #:or <any>
(define (manyUntil p end) (manyTill p end #:or <any>))
(define (many1Until p end) (many1Till p end #:or <any>))

(define (sepBy1 p sep) (parser-cons p (many (>> sep p))))
(define (sepBy p sep) (<or> (sepBy1 p sep) 
                            (return null)))

(define (endBy p end) 
  (many (parser-one (~> p) end)))

(define (between open close p)
  (parser-one open (~> p) close))
   
;; Creates a Parser that parses with p, using exp as the expected input.
;; TODO: why is exp not merged?
(define (<?> p exp)
  (λ (in)
    (define pos (file-position in))
    (define result/#f (p in))
    (when (= pos (file-position in)) (current-expected (list exp)))
    result/#f)
  #;(λ (state)
    (match (p state)
      [(Empty (Error msg)) (Empty (Error (expect msg exp)))]
      [(Empty (Ok x st msg)) (Empty (Ok x st (expect msg exp)))]
      [other other])))
(define (expect msg exp)
  (match msg
    [(Msg pos inp _) (Msg pos inp (list exp))]))

;; creates a parser that parses char c
(define (char c)
  (<?> (satisfy (curry char=? c))
       (mk-string c)))
(define (charAnyCase c)
  (<?> (satisfy (curry char-ci=? c))
       (~a (char-upcase c) " or " (char-downcase c))))
(define $letter
  (<?> (satisfy char-alphabetic?)
       "letter"))
(define $digit
  (<?> (satisfy char-numeric?)
       "digit"))
(define $alphaNum 
  (<?> (satisfy (λ (c) (or (char-alphabetic? c) (char-numeric? c)))) 
       "letter or digit"))
(define $hexDigit
  (<?> (<or> $digit
             (oneOf "abcdef")
             (oneOf "ABCDEF"))
       "hexadecimal digit"))
(define $space (<?> (satisfy char-whitespace?) "space"))
(define $spaces (<?> (skipMany $space) "white space"))
(define $anyChar (satisfy (λ _ #t)))
(define $newline (<?> (char #\newline) "new-line"))
(define $tab (<?> (char #\tab) "tab"))

;; Consume and return a string for which the parser succeeds on each
;; character.
(define (string* str p)
  (chars (string->list str) p)      
    #;(if (str-empty? str)
        (return null)
        (parser-cons (p (str-fst str))
                     (string* (str-rst str) p))))
;; Parser P must parse successfully with each c
(define (chars cs p)
  (if (null? cs)
      (return null)
      (parser-cons (p (car cs)) (chars (rest cs) p))))
(define (string str) ;case sensitive
  (string* str char))
(define (stringAnyCase str) ;case insensitive
  (string* str charAnyCase))

;; parser that only succeeds on empty input
(define $eof
  (<?>
   (λ (in)
     (define c (peek-char in))
     (cond
       [(eof-object? c)
         (current-unexpected "")
         (current-expected null)
         null]
       [else (current-unexpected "non-empty input")
             (current-expected null)
             #f]))
   #;(λ (state)
     (match state
       [(State inp pos _)
        (if (str-empty? inp) 
            (Empty (Ok null state (Msg pos "" null)))
            (Empty (Error (Msg pos "non-empty input" null))))]))
   "end-of-file"))
(define $eol (<?> (<or> (try (string "\n\r"))
                        (try (string "\r\n"))
                        (try (string "\n"))
                        (try (string "\r")))
                  "end-of-line"))

(define $identifier (<?> (many1 (<or> $letter $digit (char #\_))) "identifier"))

(define (frc e) (if (procedure? e) (e) e))

(define (format-exp exp) 
  (string-join (map frc exp) ", " #:before-last " or "))

;; An Input is one of:
;; - String, representing a filename
;; - Path p, where (path? p) = #t
;; - InputPort in, where (input-port? in) = #t

;; parse : Parser Input -> Output
;; errors have to be printed ~s, otherwise newlines get messed up
(define (parse p [inp (current-input-port)])
  (define result/#f
    (cond [(input-port? inp)
           (port-count-lines! inp)
           (current-unexpected "")
           (current-expected null)
           (user-state (hasheq))
           (p inp)]
          [(path? inp) (with-input-from-file inp (curry parse p))]
          [(string? inp) (with-input-from-string inp (curry parse p))]
          [else (raise-user-error 'parse
                 "input not input port, file path, or string file name")]))
    (or result/#f
        (let-values ([(r c pos) (port-next-location inp)])
          (parsack-error 
           (format "at ~a\nunexpected: ~s\n  expected: ~s"
                   (format-pos (Pos r (add1 c) pos)) ; 1-based col num
                   (frc (current-unexpected))
                   (format-exp (current-expected))))))
  #;(match (p (State inp (start-pos) (hasheq)))
    [(Empty (Error (Msg pos msg exp)))
     (parsack-error 
      (format "at ~a\nunexpected: ~s\n  expected: ~s"
              (format-pos pos) (frc msg) (format-exp exp)))]
    [(Consumed! (Error (Msg pos msg exp)))
     (parsack-error 
      (format "at ~a\nunexpected: ~s\n  expected: ~s"
              (format-pos pos) (frc msg) (format-exp exp)))]
    [x x]))

(define (parse-result p s)
  (parse p s)
  #;(match (parse p s)
    [(Consumed! (Ok parsed _ _)) parsed]
    [(Empty     (Ok parsed _ _)) parsed]
    [x (parsack-error (~v x))]))

;; parser compose
(define-syntax (parser-compose stx)
  (syntax-case stx (<-)
    [(_ p) #'p]
    [(_ (x <- p) e ...)
     #'(>>= p (λ (x) (parser-compose e ...)))]
    [(_ q e ...) #'(>>= q (λ (x) (parser-compose e ...)))]))

;(define-for-syntax (add-bind stx)
;  (syntax-parse stx #:datum-literals (~)
;    [(~ p) #'p]
;    [q #`(#,(generate-temporary) <- q)]))
(define-syntax (parser-seq stx)
  (define (add-bind stx)
    (syntax-parse stx #:datum-literals (~)
      [(~ p) #'p]
      [q #`(#,(generate-temporary) <- q)]))
  (syntax-parse stx #:datum-literals (~)
    [(_ p:expr ...
        (~optional (~seq #:combine-with combine:expr) #:defaults ([combine #'list])))
     (with-syntax ([(new-p ...) (map add-bind (syntax->list #'(p ...)))])
       (syntax-parse #'(new-p ...) #:datum-literals (<-)
         [(~and ((~or (x <- q1) q2) ...)
                (q ...))
          ;(printf "~a\n" (syntax->datum #'(q2 ...))) ; uncomment for debugging
          #'(parser-compose q ... (return (combine x ...)))]))]))
;;(parse (parser-seq $letter $digit) "a1")
;;(parse (parser-seq $letter $digit #:combine-with list) "a1")

(define-syntax-rule (parser-cons x y) (parser-seq x y #:combine-with cons))
;(define-syntax-rule (parser-one x ...) (parser-seq x ... #:combine-with (λ (y) y)))
(define-syntax (parser-one stx)
  (define (add-bind stx)
    (syntax-parse stx #:datum-literals (~>)
      [(~> p) #'p]
      [q #`(~ q)]))
  (syntax-parse stx #:datum-literals (~>)
    [(_ (~and (~seq (~or (~once (~> q1:expr) 
                                #:name "return parse (wrapped with ~>)"
                                #:too-many "too many parses to return (wrapped with ~>)"
                                #:too-few "missing return parse (wrapped with ~>)") 
                         (~not (~> q2:expr))) ...)
              (~seq p:expr ...)))
     (with-syntax ([(new-p ...) (map add-bind (syntax->list #'(p ...)))])
       #'(parser-seq new-p ... #:combine-with (λ (x) x)))]))

(define (choice ps) (apply <or> ps))

(define (getState key)
  (λ (in)
    (define val (hash-ref (user-state) key #f))
    (current-unexpected "")
    (current-expected null)
    val)
  #;(match-lambda
   [(and state (State _ pos user))
    (Empty (Ok (hash-ref user key #f)
               state
               (Msg pos "" null)))]))

(define (setState key val)
  (λ (in)
    (define current-val (hash-ref (user-state) key 'key-not-set))
    (current-unexpected "")
    (current-expected null)
    (user-state (hash-set (user-state) key val))
    current-val)
  #;(match-lambda
   [(and state (State inp pos user))
    (Empty (Ok (hash-ref user key #f) ;; "return" original value
               (State inp pos (hash-set user key val))
               (Msg pos "" null)))]))

;; Roughly like `parameterize`, but for user state
(define-syntax (withState stx)
  (syntax-case stx ()
    [(_ ([k v] ...) p)
     (with-syntax ([(orig ...) (generate-temporaries #'(k ...))])
       (syntax/loc stx
         (parser-compose (orig <- (setState k v)) ...
                         (result <- p)
                         (setState k orig) ...
                         (return result))))]))
