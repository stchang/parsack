#lang racket
(provide (all-defined-out))

;; A [Parser X] is a function String -> [Consumed X]

;; A [Consumed X] is one of:
;; - (Consumed [Reply X])
;; - (Empty [Reply X])
(struct Consumed (Reply) #:transparent) ;; should be lazy? (see >>= def)
(struct Empty (Reply) #:transparent)

;; A [Reply X] is one of:
;; - (Ok X String)
;; - Error
(struct Ok (fst rst) #:transparent)
(struct Err ())
(define Error (Err))
(define (Error? x) (eq? x Error))

;; creates a parser that consumes no input and returns x
(define (return x)
  (λ (input) (Empty (Ok x input))))

(define (str-empty? str) (string=? str ""))
(define (str-fst str) (string-ref str 0))
(define (str-rst str) (substring str 1))

;; creates a parser that consumes 1 char if it satisfies predicate p?
(define (satisfy p?)
  (λ (input)
    (if (str-empty? input)
        (Empty Error)
        (let ([c (str-fst input)])
          (if (p? c)
              (Consumed (Ok c (str-rst input)))
              (Empty Error))))))

(define (noneOf str)
  (define (char=any c s)
    (if (str-empty? s)
        #f
        (or (char=? c (str-fst s))
            (char=any c (str-rst s)))))
  (satisfy (λ (c) (not (char=any c str)))))

;; creates a parser that parses char c
(define (char c) (satisfy (curry char=? c)))
(define letter (satisfy char-alphabetic?))
(define digit (satisfy char-numeric?))

;; creates a parser that combines two parsers p and f
;; - if p succeeds but does not consume input, then f determines result
;; - if p succeeds and consumes input, return Consumed with thunk that delays
;;   parsing from f
(define (>>= p f)
  (λ (input)
    (match (p input)
      [(Empty reply)
       (match reply
         [(Ok x rest) ((f x) rest)]
         [(Err) (Empty Error)])]
      [(Consumed reply1)
       (Consumed ; arg to Consumed constructor should be delayed
;        (lazy
         (match reply1 ;(force reply1)
           [(Ok x rest)
            (match ((f x) rest)
              [(Consumed reply2) reply2]
              [(Empty reply2) reply2])]
           [error error]))])))
(define (>> p q) (>>= p (λ _ q)))

;; <|> choice combinator
(define (<or>2 p q)
  (λ (input)
    (match (p input)
      [(Empty (Err)) (q input)]
      [(Empty ok)
       (match (q input)
         [(Empty _) (Empty ok)]
         [consumed consumed])]
      [consumed consumed])))
;; assumes (length args) >= 2
(define (<or> . args)
  (foldl (λ (p acc) (<or>2 acc p)) (car args) (cdr args)))

;; lookahead
(define (try p)
  (λ (input)
    (match (p input)
      [(Consumed (Err)) (Empty Error)]
      [other other])))

(define (string str)
  (if (str-empty? str)
      (return null)
      (>>= (char (str-fst str)) (λ _ (string (str-rst str))))))

;; parser that only succeeds on empty input
(define (eof str) (if (str-empty? str) (Empty (Ok null "")) (Empty Error)))
(define eol (<or> (try (string "\n\r"))
                  (try (string "\r\n"))
                  (try (string "\n"))
                  (try (string "\r"))))


;; parse with p 0 or more times
(define (many p)
  (<or> (>>= p
             (λ (x) (>>= (many p) (λ (xs) (return (cons x xs))))))
        (return null)))

;; parse with p 1 or more times
(define (many1 p)
  (>>= p 
       (λ (x) (>>= (<or> (many1 p) (return null))
                   (λ (xs) (return (cons x xs)))))))


(define identifier (many1 (<or> letter (<or> digit (char #\_)))))

(define (sepBy1 p sep)
  (>>= p (λ (x) (>>= (many (>>= sep (λ _ p))) (λ (xs) (return (cons x xs)))))))
(define (sepBy p sep) (<or> (sepBy1 p sep) (return null)))

(define (endBy p end) 
  ;(<or>
   (many (>>= p (λ (x) (>>= end (λ _ (return x)))))))
   ;(return null)))

(define (parse p inp) (p inp))