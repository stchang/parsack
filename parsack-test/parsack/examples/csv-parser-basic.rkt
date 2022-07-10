#lang racket
(require parsack)
(provide (all-defined-out))

;; csv parser using basic combinators (ie not sepBy)
;; - does not support quoted cells

;; many1 means cells cannot be empty
(define $cellContent (many (noneOf ",\n\r")))

(define $remainingCells
  (<or> (>>= (char #\,) (λ _ $cells))
        (return null)))

(define $cells
  (parser-cons $cellContent $remainingCells)
  #;(parser-compose (x  <- $cellContent )
                    (xs <- $remainingCells)
                    (return (cons x xs))))

;; a line must end in \n
(define $line
  (parser-one (~> $cells) $eol) ; deliberately not using endBy
  #;(parser-compose (res <- $cells) $eol (return res)))

;; result is list of list of chars

(define $csv
  (parser-one (~> (many $line)) $eof)
  #;(>>= (many1 line) (λ (result) (>>= (char #\null) (λ _ (return result)))))
  #;(parser-compose (res <- (many $line)) $eof (return res)))

;; csvFile : Path -> Parse Result
(define (csvFile filename)
  (parse $csv filename))
