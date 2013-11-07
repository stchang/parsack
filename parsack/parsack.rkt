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

;; A State is a (State String Pos)
;; - parsers consume a State
(struct State (str pos) #:transparent)
;; where str = input
;;       pos = Pos

;; A Message is a (Msg Pos String [List String])
(struct Msg (pos str strs) #:transparent)
;; where pos = Pos
;;       str = unexpected input
;;       strs = possible expected inputs

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
  (match-lambda [(State inp pos) (Empty (Error (Msg pos inp null)))]))

;; A Pos is a (Pos ofs line col)
(struct Pos (ofs line col) #:transparent)
(define parse-source (make-parameter #f)) ;; not in Pos for efficiency
(define (start-pos)
  (Pos 0 0 0))
(define (incr-pos p c)
  (match* (p c)
    [((Pos ofs line col) #\newline) (Pos (add1 ofs) (add1 line) 0)]
    [((Pos ofs line col) _        ) (Pos (add1 ofs) line        (add1 col))]))
(define (format-pos p)
  (match* (p (parse-source))
    [((Pos ofs line col) (? path-string? src))
     (format "~a:~a:~a:~a" src (add1 line) (add1 col) (add1 ofs))]
    [((Pos ofs line col) _)
     (format "~a:~a:~a" (add1 line) (add1 col) (add1 ofs))]))

;; creates a parser that consumes no input and returns x
(define (return x)
  (match-lambda 
    [(and state (State _ pos)) 
     (Empty (Ok x state (Msg pos "" null)))]))

;; creates a parser that consumes 1 char if it satisfies predicate p?
(define (satisfy p?)
  (match-lambda 
   [(State input pos)
    (if (str-empty? input)
        (Empty (Error (Msg pos "end of input" null)))
        (let ([c (str-fst input)]
              [cs (str-rst input)])
          (if (p? c)
              (let* ([new-pos (incr-pos pos c)]
                     [new-state (State cs new-pos)])
                (Consumed (Ok c new-state (Msg new-pos "" null))))
              (Empty (Error (Msg pos (mk-string c) null))))))]))

(define (ofString ? err)
  (λ (state)
     (match ((satisfy ?) state)
       [(Consumed! (Error (Msg pos inp exp)))
        (Consumed (Error (Msg pos inp (cons (err) exp))))]
       [(Empty (Error (Msg pos inp exp)))
        (Empty (Error (Msg pos inp (cons (err) exp))))]
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
              [(Empty ok) ok]
              #;[(Empty reply2) reply2])]
           [error error])))])))
(define (>> p q) (>>= p (λ _ q)))

;; <|> choice combinator
(define (<or>2 p q)
  (λ (state)
    (match (p state)
      [(Empty (Error msg1))
       (match (q state)
         [(Empty (Error msg2)) (mergeError msg1 msg2)]
         [(Empty (Ok x inp msg2)) (mergeOk x inp msg1 msg2)]
         #;[(Consumed (Ok x inp msg2)) (mergeConsumed x inp msg1 msg2)]
         [consumed consumed])]
      [(Empty (Ok x inp msg1))
       (match (q state)
         [(Empty (Error msg2)) (mergeOk x inp msg1 msg2)]
         [(Empty (Ok _ _ msg2)) (mergeOk x inp msg1 msg2)]
         #;[(Consumed (Ok x inp msg2)) (mergeConsumed x inp msg1 msg2)]
         [consumed consumed])]
      #;[(Consumed (Ok x inp msg1))
       (match (q inp)
         [(Empty (Error msg2)) (mergeError msg1 msg2)]
         [consumed consumed])]
      [consumed consumed])))
(define (mergeConsumed x inp msg1 msg2) (Consumed (Ok x inp (merge msg1 msg2))))
(define (mergeOk x inp msg1 msg2) (Empty (Ok x inp (merge msg1 msg2))))
(define (mergeError msg1 msg2) (Empty (Error (merge msg1 msg2))))
(define/match (merge msg1 msg2)
  [((Msg pos inp exp1) (Msg _ _ exp2))
   (Msg pos inp (append exp1 exp2))])
                      
;; assumes (length args) >= 2
(define (<or> . args)
  (foldl (λ (p acc) (<or>2 acc p)) (car args) (cdr args)))

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

;; parse with p but never consume input
(define (lookAhead p)
  (match-lambda
    [(and input (State inp pos))
     (match (p input)
       [(Consumed! (Ok _ _ (Msg _ str strs))) (Empty (Ok null input (Msg pos inp strs)))]
       [emp emp])]))

(define (<!> p [q $anyChar])
  (<or> (parser-compose (x <- (try p)) (unexpected x)) q)
  #;(match-lambda
   [(and input (State inp pos))
    (match (p input)
      [(Consumed! (Ok x state msg)) 
       (Empty (Error (Msg pos inp (list (string-append "not: " inp)))))]
      [_ (Empty (Ok null input (Msg pos inp null)))])]))

;; parse with p 0 or more times
(define (many p)
  (<or> 
   ;(>>= p (λ (x) (>>= (many p) (λ (xs) (return (cons x xs))))))
   #;(parser-compose (x  <- p)
                   (xs <- (many p))
                   (return (cons x xs)))
   (parser-cons p (many p))
   (return null)))

;; parse with p 1 or more times
(define (many1 p)
;  (>>= p (λ (x) (>>= (<or> (many1 p) (return null)) (λ (xs) (return (cons x xs))))))
  #;(parser-compose (x  <- p)
                  (xs <- (<or> (many1 p) (return null)))
                  (return (cons x xs)))
  (parser-cons p (<or> (many1 p) (return null))))

(define (skipMany p) (<or> (parser-compose p (skipMany p)) (return null)))
(define (skipMany1 p) (parser-compose p (skipMany p)))

;; applies parser p zero or more times until parser end succeeds
(define (manyTill p end)
  (<or> (>> end (return null))
        #;(parser-compose (x <- p)
                        (xs <- (manyTill p end))
                        (return (cons x xs)))
        (parser-cons p (manyTill p end))))

;; applies parser p one or more times until parser end succeeds
(define (many1Till p end)
  (parser-compose (x <- p)
                  (xs <- (manyTill p end))
                  (return (cons x xs))))

(define (sepBy1 p sep)
  ;(>>= p (λ (x) (>>= (many (>>= sep (λ _ p))) (λ (xs) (return (cons x xs))))))
  #;(parser-compose (x  <- p)
                  (xs <- (many (>>= sep (λ _ p))))
                  (return (cons x xs)))
  (parser-cons p (many (>> sep p))))
(define (sepBy p sep) (<or> (sepBy1 p sep) (return null)))

(define (endBy p end) 
  (many #;(parser-compose (x <- p) end (return x))
        (parser-one (~> p) end)))
;  ;(<or> 
;   (many (>>= p (λ (x) (>>= end (λ _ (return x)))))))
;   ;(return null)))

(define (between open close p)
  #;(parser-compose open (x <- p) close (return x))
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
       [(State inp pos)
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

(define (format-exp exp) (string-join exp ", " #:before-last " or "))

;; errors have to be printed ~s, otherwise newlines get messed up
(define (parse p inp) 
  (match (p (State inp (start-pos)))
    [(Empty (Error (Msg pos msg exp)))
     (error 'parse-error 
            "at ~a\nunexpected: ~s\n  expected: ~s"
            (format-pos pos) msg (format-exp exp))]
    [(Consumed! (Error (Msg pos msg exp)))
     (error 'parse-error 
            "at ~a\nunexpected: ~s\n  expected: ~s"
            (format-pos pos) msg (format-exp exp))]
    [x x]))
  
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


(define (notFollowedBy p)
  (try (<or> (parser-compose (c <- (try p))
                             (unexpected c))
             (return null))))

(define (unexpected v)
  (match-lambda
   [(and state (State inp pos))
    (Empty (Error (Msg pos inp (list (format "not followed by: ~a" v)))))]))
