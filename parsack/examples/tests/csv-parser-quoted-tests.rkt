#lang racket
(require "../../parsack.rkt")
(require "../../tests/test-utils.rkt")
(require (rename-in "../csv-parser-quoted.rkt" [$cell $cellContent] [$line $cells]))
(require rackunit)
(require racket/runtime-path)

(define $line
  (parser-one (~> $cells) $eol)
  #;(>>= $cells (λ (res) (>> $eol (return res)))))
(define $remainingCells (<or> (>> (char #\,) $cells)
                             (return null)))

(check-parsing ($cellContent "abc") "abc" "")
(check-parsing ($cellContent "abc\n") "abc" "\n")
(check-parsing ($cellContent "abc,") "abc" ",")
(check-empty-parsing ($cellContent "\n") "\n")
(check-empty-parsing ($cellContent ",") ",")

(check-parse-error 
 ((>> (char #\,) $cellContent) "abc") (fmt-err-msg  1 1 1 "a" (list ",")))
(check-parse-error 
 ((>> (char #\,) $cellContent) "\nabc") (fmt-err-msg 1 1 1 "\n" (list ",")))
(check-parsing ((>> (char #\,) $cellContent) ",abc") "abc" "")

(check-empty-parsing ($remainingCells "abc") "abc")

(check-parsings ($cells "abc,def") "abc" "def" "")

(check-parsings ($cells "abc,def,ghi\n") "abc" "def" "ghi" "\n")
(check-parsings ($cells "abc,,ghi\n") "abc" "" "ghi" "\n")
(check-parsings ($cells "abc,,ghi") "abc" "" "ghi" "")

(check-parsings ($line "abc,def\nghi") "abc" "def" "ghi")
(check-parse-error
 ($line "abc") (fmt-err-msg 1 4 4 "end of input" (list "," "end-of-line")))

(check-empty-parsing ($csv "") "")
(check-parse-error 
 ($csv "abc") (fmt-err-msg 1 4 4 "end of input" (list "," "end-of-line")))
(check-line-parsings ($csv "abc,def\nghi,jkl\n") ("abc" "def") ("ghi" "jkl") "")

(check-line-parsings ($csv "\"This, is, one, big, cell\"\n") ("This, is, one, big, cell") "")

;; csv example from RWH: http://book.realworldhaskell.org/read/using-parsec.html
(define-runtime-path csv-example "csv-example")
(check-line-parsings ($csv (with-input-from-file csv-example port->string))
                     ("Product" "Price")
                     ("O'Reilly Socks" "10")
                     ("Shirt with \"Haskell\" text" "20")
                     ("Shirt, \"O'Reilly\" version" "20")
                     ("Haskell Caps" "15") "")

;; all Real World Haskell tests
(check-empty-parsing ($csv "") "")
(check-parse-error
 ($csv "hi") (fmt-err-msg 1 3 3 "end of input" (list "," "end-of-line")))
(check-line-parsings ($csv "hi\n") ("hi") "")
(check-line-parsings ($csv "line1\nline2\nline3\n") ("line1") ("line2") ("line3") "")
(check-line-parsings ($csv "cell1,cell2,cell3\n") ("cell1" "cell2" "cell3") "")
(check-line-parsings ($csv "l1c1,l1c2\nl2c1,l2c2\n") ("l1c1" "l1c2") ("l2c1" "l2c2") "")
(check-line-parsings ($csv "Hi,\n\n,Hello\n") ("Hi" "") ("") ("" "Hello") "")
