#lang racket
(require "string-helpers.rkt")
(require (for-syntax syntax/parse racket/syntax))
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;; Parsec (with error messages) ;;;;;;;;;;;;;;;;;;;;;;;;;
;; implements monadic combinators for LL parsing

;; In this library, an identifier prefixed with $ denotes an instantiated parser
;; (as opposed to a parser-creating function)


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

(define $err 
  (match-lambda [(State inp pos _) (Empty (Error (Msg pos inp null)))]))

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

;; creates a parser that consumes no input and returns x
(define (return x)
  (match-lambda 
    [(and state (State _ pos _))
     (Empty (Ok x state (Msg pos "" null)))]))

;; creates a parser that consumes 1 char if it satisfies predicate p?
(define (satisfy p?)
  (match-lambda 
   [(State input pos user)
    (if (str-empty? input)
        (Empty (Error (Msg pos "end of input" null)))
        (let ([c (str-fst input)]
              [cs (str-rst input)])
          (if (p? c)
              (let* ([new-pos (incr-pos pos c)]
                     [new-state (State cs new-pos user)])
                (Consumed (Ok c new-state (Msg new-pos "" null))))
              (Empty (Error (Msg pos (mk-string c) null))))))]))

(define (ofString ? err)
  (λ (state)
     (match ((satisfy ?) state)
       [(Consumed! (Error (Msg pos inp exp)))
        (Consumed (Error (Msg pos inp (cons err exp))))]
       [(Empty (Error (Msg pos inp exp)))
        (Empty (Error (Msg pos inp (cons err exp))))]
       [ok ok])))

(define (oneOf str)
  (ofString (curry char-in-string? str)
            (thunk (string-append "one of: " (str->strs str)))))
(define (noneOf str)
  (ofString (compose1 not (curry char-in-string? str))
            (thunk (string-append "none of: " (str->strs str)))))
       
(define (char-in-string? str char) ;; char is last to facilitate currying
  (for/or ([c (in-string str)])
    (char=? c char)))

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


;; creates a parser that combines two parsers p and f
;; - if p succeeds but does not consume input, then f determines result
;; - if p succeeds and consumes input, return Consumed with thunk that delays
;;   parsing from f
(define (>>= p f)
  (λ (input)
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
  (λ (state)
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
(define (<or> p . ps) 
  (foldl (λ (q acc) (<or>2 acc q)) p ps))

;; short-circuiting choice combinator
;; only tries 2nd parser q if p errors
;; differs from <or> in the case where p returns (Empty (Ok ...))
;; - <or> keeps going with q
;; - but <any> stops
(define (<any>2 p q)
  (λ (state)
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
  (λ (state)
    (match (p state)
      [(Consumed! (Error msg)) (Empty (Error msg))]
      [other other])))

;; Parse p and return the result, but don't consume input.
(define (lookAhead p)
  (match-lambda
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
  (match-lambda 
    [(and state (State inp pos _))
     (match (p state)
       [(Consumed! (Ok res _ _))
        (Empty (Error (Msg pos (thunk (result->str res)) 
                           (list (thunk (format "not: ~a" (result->str res)))))))]
       [_ (q state)])]))

(define (notFollowedBy p)
  (match-lambda
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
   

(define (<?> p exp)
  (λ (state)
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
  (if (str-empty? str)
      (return null)
      (parser-cons (p (str-fst str))
                   (string* (str-rst str) p))))
(define (string str) ;case sensitive
  (string* str char))
(define (stringAnyCase str) ;case insensitive
  (string* str charAnyCase))

;; parser that only succeeds on empty input
(define $eof
  (<?>
   (λ (state)
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

;; errors have to be printed ~s, otherwise newlines get messed up
(define (parse p inp) 
  (match (p (State inp (start-pos) (hasheq)))
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
  (match (parse p s)
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

(define (choice ps) (apply <or> $err ps))

(define (getState key)
  (match-lambda
   [(and state (State _ pos user))
    (Empty (Ok (hash-ref user key #f)
               state
               (Msg pos "" null)))]))

(define (setState key val)
  (match-lambda
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
