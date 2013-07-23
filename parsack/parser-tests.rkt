#lang racket
(require "parser.rkt")
(require "test-utils.rkt")
(require rackunit)

(check-equal? ((char #\A) "A") (Consumed (Ok #\A "")))
(check-equal? ((char #\A) "ABC") (Consumed (Ok #\A "BC")))
(check-equal? ((char #\A) "B") (Empty Error))

(check-equal? (letter "A") (Consumed (Ok #\A "")))
(check-equal? (letter "b") (Consumed (Ok #\b "")))
(check-equal? (letter "bc") (Consumed (Ok #\b "c")))
(check-equal? (letter "A1") (Consumed (Ok #\A "1")))
(check-equal? (letter "1") (Empty Error))

(check-equal? (digit "1") (Consumed (Ok #\1 "")))

(check-equal? ((noneOf "a") "a") (Empty Error))
(check-equal? ((noneOf "a") "b") (Consumed (Ok #\b "")))
(check-equal? ((noneOf "ab") "a") (Empty Error))
(check-equal? ((noneOf "ab") "b") (Empty Error))
(check-equal? ((noneOf "ab") "c") (Consumed (Ok #\c "")))

(check-empty-parsing (eof "") "")
(check-parse-error (eof "a"))
(check-parsing (eol "\n") "" "")
(check-parse-error (eol "a"))

(check-parsing ((>>= eol (λ _ eof)) "\n\r") "" "")
(check-parsing ((>>= eol (λ _ eof)) "\n") "" "")
(check-parsing ((>>= eol (λ _ eof)) "\r\n") "" "")
(check-parsing ((>>= eol (λ _ eof)) "\r") "" "")
