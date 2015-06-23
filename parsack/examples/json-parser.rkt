#lang racket
(require "../parsack.rkt")
(provide (all-defined-out))

;; A JValue is one of:
;; - (JString String)
;; - (JNumber Number)
;; - (JBool Boolean)
;; - JNull
;; - (JObject [Listof [Pairof String JValue]])
;; - (JArray [Listof JValue])
(struct JObject (kvs) #:transparent)
(struct JArray (vs) #:transparent)
(struct JString (str) #:transparent)
(struct JNumber (num) #:transparent)
(struct JBool (bool) #:transparent)
(struct JNull () #:transparent)

;; todo: unicode parsing
(define $jchar
  (<or> (parser-compose
         (char #\\)
         (<or> (>> (char #\b) (return #\backspace))
               (>> (char #\n) (return #\newline))
               (>> (char #\f) (return #\page))
               (>> (char #\r) (return #\return))
               (>> (char #\t) (return #\tab))
               (>> (char #\\) (return #\\))
               (>> (char #\") (return #\"))
               (>> (char #\/) (return #\/))
               ;; unicode
               ))
         (noneOf "\"\\")))

(define $p_string (between (char #\") (char #\") (many $jchar)))


(define/match (pos+ p m)
  [((Pos ln col n) m) (Pos ln (+ col m) (+ n m))])

;; uses Racket's read to read a datum
;; - if it's a number, return it
;; - if it's not a number, don't consume any input
(define $p_number
  (λ (in)
    (define n (read (peeking-input-port in)))
    (if (number? n)
        (Consumed (Ok (read in)))
        (Empty (Error))))
  #;(λ (state)
    (define s (State-str state))
    (define p (open-input-string s))
    (define n (read p))
    (define pos (file-position p))
    (define rst (port->string p))
    (if (number? n)
        (Consumed (Ok n (State rst (pos+ (State-pos state) pos) (State-user state))
                      (Msg (Pos 1 1 1) "" null)))
        (Empty (Error (Msg (Pos 1 1 1) "number" null))))))

(define $p_bool (<or> (>> (string "true") (return #t))
                      (>> (string "false") (return #f))))

(define (<$> f p)
  (parser-compose
   (x <- p)
   (return (f x))))
(define ($value state)
  ((<?> (<or> (<$> (λ (cs) (JString (list->string cs))) $p_string)
              (<$> JNumber $p_number)
              (<$> JObject $p_object)
              (<$> JArray $p_array)
              (<$> JBool $p_bool)
              (>> (string "null") (return (JNull))))
        "JSON value")
   state))
(define $p_value (>> $spaces $value))

(define $p_field 
  (parser-compose
   (k <- $p_string)
   (char #\:)
   $spaces
   (v <- $p_value)
   (return (cons (list->string k) v))))

(define (p_series left p right)
  (between (parser-one (~> (char left)) $spaces)
           (char right)
           (sepBy (parser-one (~> p) $spaces)
                  (parser-one (~> (char #\,)) $spaces))))

(define $p_array (p_series #\[ $p_value #\]))
(define $p_object (p_series #\{ $p_field #\}))

(define $text
  (<or> (parser-compose
         (kvs <- $p_object)
         (return (JObject kvs)))
        (parser-compose
         (vs <- $p_array)
         (return (JArray vs)))))

;; parses json
(define $p_text (<?> (parser-one $spaces (~> $text)) "JSON text"))
   