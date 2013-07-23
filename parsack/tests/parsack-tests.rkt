#lang racket
(require "../parsack.rkt")
(require "test-utils.rkt")
(require rackunit)

;; parse-error: at pos 0
;; unexpected @: expected letter, digit or _
(check-exn exn:fail? (thunk (parse $identifier "")))

;; parse-error: at pos 0
;; unexpected @: expected identifier
(check-exn exn:fail? (thunk (parse $identifier "@")))

;; parse-error: at pos 0
;; unexpected *: expected digit or letter
(check-exn exn:fail?
           (thunk (parse (>> (<or> $digit (return #\0)) $letter) "*")))


(check-parsing ((char #\A) "A") "A" "")
(check-parsing ((char #\A) "ABC") "A" "BC")
(check-parse-error 
 ((char #\A) "B")
 "parse-error: at pos 0\nunexpected B: expected \"A\"")

(check-parsing ($letter "A") "A" "")
(check-parsing ($letter "b") "b" "")
(check-parsing ($letter "bc") "b" "c")
(check-parsing ($letter "A1") "A" "1")
(check-parse-error 
 ($letter "1")
 "parse-error: at pos 0\nunexpected 1: expected letter")

(check-parsing ($digit "1") "1" "")

(check-parse-error 
 ((noneOf "a") "a")
 "parse-error: at pos 0\nunexpected a: expected none of \"a\"")
(check-parsing ((noneOf "a") "b") "b" "")
(check-parse-error 
 ((noneOf "ab") "a")
 "parse-error: at pos 0\nunexpected a: expected none of \"a\" or \"b\"")
(check-parse-error 
 ((noneOf "ab") "b")
 "parse-error: at pos 0\nunexpected b: expected none of \"a\" or \"b\"")
(check-parsing ((noneOf "ab") "c") "c" "")

(check-empty-parsing ($eof "") "")
(check-parse-error 
 ($eof "a")
 "parse-error: at pos 0\nunexpected non-empty input: expected end-of-file")
(check-parsing ($eol "\n") "" "")
(check-parse-error 
 ($eol "a")
 "parse-error: at pos 0\nunexpected a: expected end-of-line")

(check-parsing ((>> $eol $eof) "\n\r") "" "")
(check-parsing ((>> $eol $eof) "\n") "" "")
(check-parsing ((>> $eol $eof) "\r\n") "" "")
(check-parsing ((>> $eol $eof) "\r") "" "")
