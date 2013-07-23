#lang racket
(require "parser-with-errors.rkt")
(provide (all-defined-out))


;(define eol (char #\newline))

;; many1 means cells cannot be empty
(define cellContent (many (noneOf ",\n\r")))

(define remainingCells
  (<or> (>>= (char #\,) (λ _ cells))
        (return null)))

(define cells
  (>>= cellContent (λ (x) (>>= remainingCells (λ (xs) (return (cons x xs)))))))

;; a line must end in \n
(define line
  (>>= cells (λ (result) (>>= eol (λ _ (return result))))))

;; result is list of list of chars

(define csv
;  (>>= (many1 line) (λ (result) (>>= (char #\null) (λ _ (return result))))))
  (>>= (many line) (λ (result) (>>= eof (λ _ (return result))))))


(define (csvFile filename)
  (parse csv (with-input-from-file filename port->string)))
