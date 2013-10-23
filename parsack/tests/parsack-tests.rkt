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
(check-parse-error ((char #\A) "B") (fmt-err-msg 0 "B" (list "A")))

(check-parsing ($letter "A") "A" "")
(check-parsing ($letter "b") "b" "")
(check-parsing ($letter "bc") "b" "c")
(check-parsing ($letter "A1") "A" "1")
(check-parse-error ($letter "1") (fmt-err-msg 0 "1" "letter"))

(check-parsing ($digit "1") "1" "")

(check-parse-error ((noneOf "a") "a") (fmt-err-msg 0 "a" (list "a") #:extra "none of"))
(check-parsing ((noneOf "a") "b") "b" "")
(check-parse-error ((noneOf "ab") "a") (fmt-err-msg 0 "a" (list "a" "b") #:extra "none of"))
(check-parse-error ((noneOf "ab") "b") (fmt-err-msg 0 "b" (list "a" "b") #:extra "none of"))
(check-parsing ((noneOf "ab") "c") "c" "")

(check-empty-parsing ($eof "") "")
(check-parse-error ($eof "a") (fmt-err-msg 0 "non-empty-input" "end-of-file"))
(check-parsing ($eol "\n") "" "")
(check-parse-error ($eol "a") (fmt-err-msg 0 "a" "end-of-line"))

(check-parsing ((>> $eol $eof) "\n\r") "" "")
(check-parsing ((>> $eol $eof) "\n") "" "")
(check-parsing ((>> $eol $eof) "\r\n") "" "")
(check-parsing ((>> $eol $eof) "\r") "" "")

(check-empty-parsing ((many $letter) "") "")
(check-parsing ((many $letter) "a") "a" "")
(check-parsing ((many $letter) "aa") "aa" "")
(check-parsing ((many $letter) "abc") "abc" "")

(check-parsing ((parser-one (~> $letter) $digit) "a1") "a" "")
;(parse (parser-one (~> $letter) (~> $digit)) "a1")
; error: too many parses
;(parse (parser-one $letter $digit) "a1")
; error: too few parses

(check-parse-error ($err "any") (fmt-err-msg 0 "any" ""))
(check-parse-error 
 ((>> (lookAhead $tab) (<or> $letter $digit)) "\t")
 (fmt-err-msg 0  "\t" (list "letter" "digit")))
(check-parse-error 
 ((>> (lookAhead $tab) (<or> $letter $digit)) "A")
 (fmt-err-msg 0  "A" (list "tab")))
(check-empty-parsing ((lookAhead (string "A\n")) "A\n") "A\n")
(check-parsing ((>> (lookAhead (string "A\n")) $letter) "A\n") "A" "\n")
(check-parse-error 
 ((>> (<!> (string "A\n")) (<or> $letter $digit)) "A\n")
 (fmt-err-msg 0 "A\n" (list "A\n") #:extra "not"))
(check-parse-error 
 ((>> (<!> (string "A\n")) (<or> $letter $digit)) "A\n\n")
 (fmt-err-msg 0 "A\n\n" (list "A\n\n") #:extra "not"))
(check-parsing ((>> (<!> (string "A\n")) (<or> $letter $digit)) "AB") "A" "B")
(check-parsing ((>> (<!> (string "A\n")) (<or> $letter $digit)) "BA") "B" "A")
(check-parsing ((>> (<!> (string "A\n")) (<or> $letter $digit)) "1A") "1" "A")
 
