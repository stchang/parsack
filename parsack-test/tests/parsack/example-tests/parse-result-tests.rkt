#lang racket
(require parsack
         parsack/examples/json-parser
         rackunit)

(check-equal? (parse-result $eof "") null)
(check-equal? (parse-result $letter "A") #\A)
(check-equal? (parse-result (string "what") "what?") '(#\w #\h #\a #\t))
(check-equal? (parse-result $p_text "[-3.14, true, null, \"a string\"]")
              (JArray (list (JNumber -3.14)
                            (JBool #t)
                            (JNull)
                            (JString "a string"))))
