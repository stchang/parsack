#lang racket
(require "../parsack.rkt")
(require "test-utils.rkt")
(require rackunit)

;; parse-error: at pos 0
;; unexpected @: expected letter, digit or _
(check-exn exn:fail:parsack? (thunk (parse $identifier "")))

;; parse-error: at pos 0
;; unexpected @: expected identifier
(check-exn exn:fail:parsack? (thunk (parse $identifier "@")))

;; parse-error: at pos 0
;; unexpected *: expected digit or letter
(check-exn exn:fail:parsack?
           (thunk (parse (>> (<or> $digit (return #\0)) $letter) "*")))


(check-parsing ((char #\A) "A") "A" "")
(check-parsing ((char #\A) "ABC") "A" "BC")
(check-parse-error ((char #\A) "B")
                   (fmt-err-msg 1 1 1 "B" (list "A")))

(check-parsing ((charAnyCase #\A) "A") "A" "")
(check-parsing ((charAnyCase #\A) "a") "a" "")
(check-parse-error ((charAnyCase #\A) "Z")
                   (fmt-err-msg 1 1 1 "Z" (list "A" "a")))

(check-parsing ((string "ABC") "ABC") "ABC" "")
(check-parsing ((string "ABC") "ABCxxx") "ABC" "xxx")
(check-parse-error ((string "ABC") "xzy")
                   (fmt-err-msg 1 1 1 "x" (list "A")))

(check-parsing ((stringAnyCase "ABC") "ABC") "ABC" "")
(check-parsing ((stringAnyCase "ABC") "abc") "abc" "")
(check-parsing ((stringAnyCase "ABC") "ABC___") "ABC" "___")
(check-parsing ((stringAnyCase "ABC") "abc___") "abc" "___")
(check-parse-error ((stringAnyCase "ABC") "xzy")
                   (fmt-err-msg 1 1 1 "x" (list "A" "a")))
(check-parse-error ((stringAnyCase "ABC") "xzy")
                   (fmt-err-msg 1 1 1 "x" (list "A" "a")))

(check-parsing ($letter "A") "A" "")
(check-parsing ($letter "b") "b" "")
(check-parsing ($letter "bc") "b" "c")
(check-parsing ($letter "A1") "A" "1")
(check-parsing ($alphaNum "A1") "A" "1")
(check-parse-error ($letter "1") (fmt-err-msg 1 1 1 "1" (list "letter")))

(check-parsing ($digit "1") "1" "")
(check-parsing ($alphaNum "1") "1" "")
(check-parse-error ($alphaNum "!")
                   (fmt-err-msg 1 1 1 "!" (list "letter or digit")))

(check-parse-error ((noneOf "a") "a")
                   (fmt-err-msg 1 1 1 "a" (list "a") #:extra "none of"))
(check-parsing ((noneOf "a") "b") "b" "")
(check-parse-error ((noneOf "ab") "a")
                   (fmt-err-msg 1 1 1 "a" (list "a" "b") #:extra "none of"))
(check-parse-error ((noneOf "ab") "b")
                   (fmt-err-msg 1 1 1 "b" (list "a" "b") #:extra "none of"))
(check-parsing ((noneOf "ab") "c") "c" "")

(check-empty-parsing ($eof "") "")
(check-parse-error ($eof "a")
                   (fmt-err-msg 1 1 1 "non-empty input" (list "end-of-file")))
(check-parsing ($eol "\n") "\n" "")
(check-parse-error ($eol "a")
                   (fmt-err-msg 1 1 1 "a" (list "end-of-line")))

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

(check-parse-error ($err "any") (fmt-err-msg 1 1 1 "any" (list "")))
(check-parse-error 
 ((>> (lookAhead $tab) (<or> $letter $digit)) "\t")
 (fmt-err-msg 1 1 1 "\t" (list "letter" "digit")))
(check-parse-error 
 ((>> (lookAhead $tab) (<or> $letter $digit)) "A")
 (fmt-err-msg 1 1 1 "A" (list "tab")))
(check-empty-parsing ((lookAhead (string "A\n")) "A\n") "A\n" "A\n")
(check-parsing ((>> (lookAhead (string "A\n")) $letter) "A\n") "A" "\n")
(check-parse-error ((<!> (string "A\n")) "A\n")
                   (fmt-err-msg 1 1 1 "A\n" (list "A\n") #:extra "not"))
(check-parse-error ((<!> (string "A\n")) "A\n\n")
                   (fmt-err-msg 1 1 1 "A\n" (list "A\n") #:extra "not"))
(check-parsing ((<!> (string "A\n")) "AB") "A" "B")
(check-parsing ((<!> (string "A\n")) "BA") "B" "A")
(check-parsing ((<!> (string "A\n")) "1A") "1" "A")
 
(check-parse-error ((parser-seq (char #\a) (notFollowedBy (char #\b))) "ab")
                   (fmt-err-msg 1 2 2 "b" (list "b") #:extra "not"))
(check-parsing ((parser-seq (char #\a) (~ (notFollowedBy (char #\b)))) "ac") "a" "c")

(check-parse-error ((parser-seq (char #\a) (notFollowedBy (string "bc"))) "abc")
                   (fmt-err-msg 1 2 2 "bc" (list "bc") #:extra "not"))
(check-parsing ((parser-seq (char #\a) (~ (notFollowedBy (string "bc")))) "abd") "a" "bd")

(check-parsing ((parser-one (~> (string "let")) (notFollowedBy $alphaNum)) "let ") "let" " ")
(check-parse-error ((parser-one (~> (string "let")) (notFollowedBy $alphaNum)) "lets")
                   (fmt-err-msg 1 4 4 "s" (list "s") #:extra "not"))
;; test manytill
(check-parsing ((manyTill (string "one") (string "two")) "two") "" "")
(check-parse-error ((many1Till (string "one") (string "two")) "two")
                   (fmt-err-msg 1 1 1 "t" (list "o")))
(check-parsings ((manyTill (string "one") (string "two")) "onetwo") "one" "")
(check-parsings ((manyTill (string "one") (string "two")) "oneonetwo") "one" "one" "")
(check-parsings ((many1Till (string "one") (string "two")) "onetwo") "one" "")
(check-parsings ((many1Till (string "one") (string "two")) "oneonetwo") "one" "one" "")
(check-parse-error ((manyTill (string "one") (string "two")) "ontwo")
                   (fmt-err-msg 1 3 3 "t" (list "e")))
(check-parse-error ((many1Till (string "one") (string "two")) "ontwo")
                   (fmt-err-msg 1 3 3 "t" (list "e")))

;; more manyTill and manyUntil tests
;; via greghendershott, see https://github.com/stchang/parsack/issues/34
(check-parse-error ((manyTill $anyChar (lookAhead (char #\X))) "abcX")
                   (fmt-err-msg 1 5 5 "end of input" (list "X")))
(check-parsing ((manyUntil $anyChar (lookAhead (char #\X))) "abcX") "abc" "X")

;; html list example
(define ul-example "<ul><li>Zero</li><li>One<li>Two</ul>")
(define (open-tag name) 
  (try (parser-one (char #\<) (~> (string name)) (char #\>))))
(define (close-tag name) 
  (try (parser-one (char #\<) (char #\/) (~> (string name)) (char #\>))))
(define $li-element ; ok because it uses manyUntil
  (>> (open-tag "li") 
      (manyUntil $anyChar    ; <----- replaced $html-element
                (<or> (close-tag "li")
                      (lookAhead (<or> (open-tag "li") 
                                       (close-tag "ul") 
                                       (close-tag "ol")))))))
(define $li-element/manyTill ; bad -- will error
  (>> (open-tag "li") 
      (manyTill $anyChar    ; <----- replaced $html-element
                (<or> (close-tag "li")
                      (lookAhead (<or> (open-tag "li") 
                                       (close-tag "ul") 
                                       (close-tag "ol")))))))
(define $li-element/manyTillany ; ok because it uses manyTill #:or <any>
  (>> (open-tag "li") 
      (manyTill $anyChar    ; <----- replaced $html-element
                (<or> (close-tag "li")
                      (lookAhead (<or> (open-tag "li") 
                                       (close-tag "ul") 
                                       (close-tag "ol"))))
                #:or <any>)))
(define $li-element/many ; ok bc it uses many #:or <any>
  (>> (open-tag "li") 
      (many $anyChar    ; <----- replaced $html-element
            #:till
            (<or> (close-tag "li")
                  (lookAhead (<or> (open-tag "li") 
                                   (close-tag "ul") 
                                   (close-tag "ol"))))
            #:or <any>)))
(define $ul-element
  (>> (open-tag "ul")
      (manyTill $li-element (close-tag "ul"))))
(define $ul-element/many
  (>> (open-tag "ul")
      (many $li-element/many #:till (close-tag "ul"))))
(define $ul-element/manyTill
  (>> (open-tag "ul")
      (manyTill $li-element/manyTill (close-tag "ul"))))
(define $ul-element/manyTillany
  (>> (open-tag "ul")
      (manyTill $li-element/manyTillany (close-tag "ul"))))
(check-parsings ($ul-element ul-example) "Zero" "One" "Two" "")
(check-parsings ($ul-element/many ul-example) "Zero" "One" "Two" "")
(check-parsings ($ul-element/manyTillany ul-example) "Zero" "One" "Two" "")
(check-parse-error ($ul-element/manyTill ul-example)
                   (fmt-err-msg 1 37 37 "end of input" (list "<" "<" "<" "<")))

;; many with #:or <any> is weird because of the default (return 0)
;; it returns right away without consuming input
(check-empty-parsing ((many $anyChar #:or <any>) "abc") "abc")


(check-parsing ((oneOfStrings "foo" "bar" "baz") "bar") "bar" "")
(check-parsing ((oneOfStrings "foo" "bar" "baz") "bar___") "bar" "___")
(check-parse-error ((oneOfStrings "foo" "bar" "baz") "BAR")
                   "parse ERROR: at 1:1:1\nunexpected: \"B\"\n  expected: \"one of: \\\"foo\\\", \\\"bar\\\", \\\"baz\\\"\"")

(check-parsing ((oneOfStringsAnyCase "foo" "bar" "baz") "BAR") "BAR" "")
(check-parsing ((oneOfStringsAnyCase "foo" "bar" "baz") "BAR___") "BAR" "___")
(check-parse-error ((oneOfStrings "foo" "bar" "baz") "XXX")
                   "parse ERROR: at 1:1:1\nunexpected: \"X\"\n  expected: \"one of: \\\"foo\\\", \\\"bar\\\", \\\"baz\\\"\"")

;; check proper "unexpected" on partial parse + fail
(check-parse-error ((string "ac") "ab") (fmt-err-msg 1 2 2 "b" (list "c")))

;; user state
(check-equal?
 (parse-result (parser-compose (setState 'key "val")
                               $anyChar
                               (x <- (getState 'key))
                               (return x))
               "whatever")
 "val")
(check-equal?
 (parse-result (parser-compose (setState 'key "val")
                               (withState (['key "new-val"])
                                          $anyChar)
                               (x <- (getState 'key))
                               (return x))
               "whatever")
 "val")

;; tests for error msg merging
(check-parse-error ((<or> (char #\a) (char #\b)) "c")
                   (fmt-err-msg 1 1 1 "c" (list "a" "b")))
(check-parse-error ((>> (many (char #\,)) $eol) "a")
                   (fmt-err-msg 1 1 1 "a" (list "," "end-of-line")))
(check-parse-error ((parser-seq (many (char #\,)) $eol) "a")
                   (fmt-err-msg 1 1 1 "a" (list "," "end-of-line")))
(check-parse-error ((parser-one
                     (~> (parser-cons (many (noneOf ",\n\r"))
                                      (<or> (char #\,)
                                            (return null))))
                     $eol)
                    "ab")
                   (fmt-err-msg 1 3 3 "end of input" (list "," "end-of-line")))
