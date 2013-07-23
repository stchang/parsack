#lang racket
(require "../../parsack.rkt")
(require "../../tests/test-utils.rkt")
(require (rename-in "../csv-parser-quoted.rkt" [$cell $cellContent] [$line $cells]))
(require rackunit)

(define $line (>>= $cells (λ (res) (>> $eol (return res)))))
(define $remainingCells (<or> (>> (char #\,) $cells)
                             (return null)))

(check-parsing ($cellContent "abc") "abc" "")
(check-parsing ($cellContent "abc\n") "abc" "\n")
(check-parsing ($cellContent "abc,") "abc" ",")
(check-empty-parsing ($cellContent "\n") "\n")
(check-empty-parsing ($cellContent ",") ",")

(check-parse-error ((>> (char #\,) $cellContent) "abc")
                   "parse-error: at pos 0\nunexpected a: expected \",\"")
(check-parse-error ((>> (char #\,) $cellContent) "\nabc")
                   "parse-error: at pos 0\nunexpected \n: expected \",\"")
(check-parsing ((>> (char #\,) $cellContent) ",abc") "abc" "")

(check-empty-parsing ($remainingCells "abc") "abc")

(check-parsings ($cells "abc,def") "abc" "def" "")

(check-parsings ($cells "abc,def,ghi\n") "abc" "def" "ghi" "\n")
(check-parsings ($cells "abc,,ghi\n") "abc" "" "ghi" "\n")
(check-parsings ($cells "abc,,ghi") "abc" "" "ghi" "")

(check-parsings ($line "abc,def\nghi") "abc" "def" "ghi")
(check-partial-parse-error ($line "abc")
                           "parse-error: at pos 3\nunexpected end of input: expected \",\" or end-of-line")


(check-empty-parsing ($csv "") "")
(check-partial-parse-error ($csv "abc")
                           "parse-error: at pos 3\nunexpected end of input: expected \",\" or end-of-line")
(check-line-parsings ($csv "abc,def\nghi,jkl\n") ("abc" "def") ("ghi" "jkl") "")

(check-line-parsings ($csv "\"This, is, one, big, cell\"\n") ("This, is, one, big, cell") "")

;; csv example from RWH: http://book.realworldhaskell.org/read/using-parsec.html
(check-line-parsings ($csv (with-input-from-file "csv-example" port->string))
                     ("Product" "Price")
                     ("O'Reilly Socks" "10")
                     ("Shirt with \"Haskell\" text" "20")
                     ("Shirt, \"O'Reilly\" version" "20")
                     ("Haskell Caps" "15") "")

;; all Real World Haskell tests
(check-empty-parsing ($csv "") "")
(check-partial-parse-error ($csv "hi")
                           "parse-error: at pos 2\nunexpected end of input: expected \",\" or end-of-line")
(check-line-parsings ($csv "hi\n") ("hi") "")
(check-line-parsings ($csv "line1\nline2\nline3\n") ("line1") ("line2") ("line3") "")
(check-line-parsings ($csv "cell1,cell2,cell3\n") ("cell1" "cell2" "cell3") "")
(check-line-parsings ($csv "l1c1,l1c2\nl2c1,l2c2\n") ("l1c1" "l1c2") ("l2c1" "l2c2") "")
(check-line-parsings ($csv "Hi,\n\n,Hello\n") ("Hi" "") ("") ("" "Hello") "")
