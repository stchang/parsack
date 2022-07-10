#lang racket
(require parsack)
(provide (all-defined-out))

;; csv parser using sepBy combinator
;; - does not support quoted cells

;; cellContent in csv-parser.rkt
(define $cell (many (noneOf ",\n\r")))

;; a line must end in \n
(define $line (sepBy $cell (char #\,)))

;; result is list of list of chars
(define $csv (endBy $line $eol))

;; csvFile : Path -> String
(define (csvFile filename)
  (parse $csv filename))
