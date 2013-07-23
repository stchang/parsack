#lang racket
(require "parser-with-errors.rkt")
(require "test-utils-with-errors.rkt")
(require rackunit)

;; parse-error: at pos 0
;; unexpected @: expected letter, digit or _
(check-exn exn:fail? (thunk (parse identifier "")))

;; parse-error: at pos 0
;; unexpected @: expected identifier
(check-exn exn:fail? (thunk (parse identifier "@")))

;; parse-error: at pos 0
;; unexpected *: expected digit or letter
(check-exn exn:fail?
           (thunk (parse (>>= (<or> digit (return #\0)) (λ _ letter)) "*")))


;; tests from parser-tests.rkt
;(check-equal? ((char #\A) "A") (Consumed (Ok #\A "")))
;(check-equal? ((char #\A) "ABC") (Consumed (Ok #\A "BC")))
;(check-equal? ((char #\A) "B") (Empty Error))
(check-parsing ((char #\A) "A") "A" "")
(check-parsing ((char #\A) "ABC") "A" "BC")
(check-parse-error 
 ((char #\A) "B")
 "parse-error: at pos 0\nunexpected B: expected \"A\"")

;(check-equal? (letter "A") (Consumed (Ok #\A "")))
;(check-equal? (letter "b") (Consumed (Ok #\b "")))
;(check-equal? (letter "bc") (Consumed (Ok #\b "c")))
;(check-equal? (letter "A1") (Consumed (Ok #\A "1")))
;(check-equal? (letter "1") (Empty Error))
(check-parsing (letter "A") "A" "")
(check-parsing (letter "b") "b" "")
(check-parsing (letter "bc") "b" "c")
(check-parsing (letter "A1") "A" "1")
(check-parse-error 
 (letter "1")
 "parse-error: at pos 0\nunexpected 1: expected letter")

;(check-equal? (digit "1") (Consumed (Ok #\1 "")))
(check-parsing (digit "1") "1" "")

;(check-equal? ((noneOf "a") "a") (Empty Error))
;(check-equal? ((noneOf "a") "b") (Consumed (Ok #\b "")))
;(check-equal? ((noneOf "ab") "a") (Empty Error))
;(check-equal? ((noneOf "ab") "b") (Empty Error))
;(check-equal? ((noneOf "ab") "c") (Consumed (Ok #\c "")))
(check-parse-error 
 ((noneOf "a") "a")
 "parse-error: at pos 0\nunexpected a: expected none of \"a\"")
(check-parsing ((noneOf "a") "b") "b" "")
(check-parse-error 
 ((noneOf "ab") "a")
 "parse-error: at pos 0\nunexpected a: expected none of \"a\" or \"b\"")
(check-parse-error 
 ((noneOf "ab") "b")
 "parse-error: at pos 0\nunexpected b: expected none of \"a\" or \"b\"")
(check-parsing ((noneOf "ab") "c") "c" "")

(check-empty-parsing (eof "") "")
(check-parse-error 
 (eof "a")
 "parse-error: at pos 0\nunexpected non-empty input: expected end-of-file")
(check-parsing (eol "\n") "" "")
(check-parse-error 
 (eol "a")
 "parse-error: at pos 0\nunexpected a: expected end-of-line")

(check-parsing ((>>= eol (λ _ eof)) "\n\r") "" "")
(check-parsing ((>>= eol (λ _ eof)) "\n") "" "")
(check-parsing ((>>= eol (λ _ eof)) "\r\n") "" "")
(check-parsing ((>>= eol (λ _ eof)) "\r") "" "")
