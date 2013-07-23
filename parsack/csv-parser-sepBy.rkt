#lang racket
(require "parser.rkt")
(provide (all-defined-out))

;; simplified csv parser using sepBy
;(define eol (char #\newline))

;; cellContent in csv-parser.rkt
(define cell (many (noneOf ",\n")))

;; a line must end in \n
(define line (sepBy cell (char #\,)))

;; result is list of list of chars
(define csv (endBy line eol))

(define (csvFile filename) (csv (with-input-from-file filename port->string)))
