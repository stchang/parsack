#lang racket
(require parsack rackunit "test-utils.rkt")

(check-equal? (parse-result (byte 97) "a") 97)
(check-parse-error
 ((byte 97) "b")
 (fmt-err-msg 1 1 1 "b" (list "a")))

(check-equal?
 (bytes->string/utf-8 (apply bytes (parse-result (bytestring #"apple") "apple")))
 "apple")

(check-equal?
 (bytes->string/utf-8 (apply bytes (parse-result (bytestring #"apple") "apples")))
 "apple")

(check-parse-error
 ((bytestring #"apple") "appl")
 (fmt-err-msg 1 5 5 "end of input" (list "e")))
(check-parse-error
 ((bytestring #"apple") "appd")
 (fmt-err-msg 1 4 4 "d" (list "l")))