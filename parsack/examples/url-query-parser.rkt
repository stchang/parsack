#lang racket
(require "../parsack.rkt")
(provide (all-defined-out))

(define ASCII-ZERO (char->integer #\0))

;; [0-9A-Fa-f] -> Number from 0 to 15
(define (hex-char->number c)
  (if (char-numeric? c)
      (- (char->integer c) ASCII-ZERO)
      (match c
        [(or #\a #\A) 10]
        [(or #\b #\B) 11]
        [(or #\c #\C) 12]
        [(or #\d #\D) 13]
        [(or #\e #\E) 14]
        [(or #\f #\F) 15]
        [_ (error 'hex-char->number "invalid hex char: ~a\n" c)])))

(define (hex-string->bytes str) (list->bytes (hex-string->bytelist str)))
(define (hex-string->bytelist str)
  (with-input-from-string
   str
   (thunk
    (let loop ()
      (define c1 (read-char))
      (define c2 (read-char))
      (cond [(eof-object? c1) null]
            [(eof-object? c2) (list (hex-char->number c1))]
            [else (cons (+ (* (hex-char->number c1) 16)
                           (hex-char->number c2))
                        (loop))])))))


(define $p_hex
  (parser-compose
   (char #\%)
   (a <- $hexDigit)
   (b <- $hexDigit)
   (return 
    (integer->char (+ (* 16 (hex-char->number a)) (hex-char->number b)))))
  #;(>>= (char #\%) (λ _ (>>= hexDigit (λ (a) (>>= hexDigit (λ (b) (hex->byte-string (list a b)))))))))

(define urlBaseChars "$-_.!*'(),")

(define $p_char (<or> $letter
                      $digit
                      (oneOf urlBaseChars)
                      (>> (char #\+) (return #\space))
                      $p_hex))

(define $p_pair (parser-cons (many1 $p_char)
                             (optionMaybe (>> (char #\=) (many $p_char)))))
         
(define $p_query (sepBy $p_pair (char #\&)))