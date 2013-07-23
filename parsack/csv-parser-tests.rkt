#lang racket
(require "parser.rkt")
(require "csv-parser.rkt")
(require "test-utils.rkt")
(require rackunit)


(check-parsing (cellContent "abc") "abc" "")
(check-parsing (cellContent "abc\n") "abc" "\n")
(check-parsing (cellContent "abc,") "abc" ",")
(check-empty-parsing (cellContent "\n") "\n")
(check-empty-parsing (cellContent ",") ",")

(check-parse-error ((>>= (char #\,) (λ (_) cellContent)) "abc"))
(check-parse-error ((>>= (char #\,) (λ (_) cellContent)) "\nabc"))
(check-parsing ((>>= (char #\,) (λ (_) cellContent)) ",abc") "abc" "")

(check-empty-parsing (remainingCells "abc") "abc")

(check-parsings (cells "abc,def") "abc" "def" "")

(check-parsings (cells "abc,def,ghi\n") "abc" "def" "ghi" "\n")
(check-parsings (cells "abc,,ghi\n") "abc" "" "ghi" "\n")
(check-parsings (cells "abc,,ghi") "abc" "" "ghi" "")

(check-parsings (line "abc,def\nghi") "abc" "def" "ghi")
(check-partial-parse-error (line "abc"))

(check-empty-parsing (csv "") "")
(check-partial-parse-error (csv "abc"))
(check-line-parsings (csv "abc,def\nghi,jkl\n") ("abc" "def") ("ghi" "jkl") "")

;; csv example from RWH: http://book.realworldhaskell.org/read/using-parsec.html
(check-line-parsings (csv (with-input-from-file "csv-example" port->string))
                     ("\"Product\"" "\"Price\"")
                     ("\"O'Reilly Socks\"" "10")
                     ("\"Shirt with \"\"Haskell\"\" text\"" "20")
                     ("\"Shirt" " \"\"O'Reilly\"\" version\"" "20")
                     ("\"Haskell Caps\"" "15") "")

(check-line-parsings (csvFile "csv-example")
                     ("\"Product\"" "\"Price\"")
                     ("\"O'Reilly Socks\"" "10")
                     ("\"Shirt with \"\"Haskell\"\" text\"" "20")
                     ("\"Shirt" " \"\"O'Reilly\"\" version\"" "20")
                     ("\"Haskell Caps\"" "15") "")


;; all RWH tests
(check-empty-parsing (csv "") "")
(check-partial-parse-error (csv "hi"))
(check-line-parsings (csv "hi\n") ("hi") "")
(check-line-parsings (csv "line1\nline2\nline3\n") ("line1") ("line2") ("line3") "")
(check-line-parsings (csv "cell1,cell2,cell3\n") ("cell1" "cell2" "cell3") "")
(check-line-parsings (csv "l1c1,l1c2\nl2c1,l2c2\n") ("l1c1" "l1c2") ("l2c1" "l2c2") "")
(check-line-parsings (csv "Hi,\n\n,Hello\n") ("Hi" "") ("") ("" "Hello") "")
