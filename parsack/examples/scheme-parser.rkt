#lang racket

;; This parsack parser is roughly equivalent to the parsec parser in this
;; Haskell tutorial: http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html
;; The parser currently handles a r5rs-like scheme without supporting the
;; full numerical tower.

(require parsack)

(provide parse-expression
	 (struct-out scheme-bool)
	 (struct-out scheme-string)
	 (struct-out scheme-char)
	 (struct-out scheme-atom)
	 (struct-out scheme-number))

(struct scheme-string (val) #:transparent)
(struct scheme-bool (val) #:transparent)
(struct scheme-atom (val) #:transparent)
(struct scheme-char (val) #:transparent)
(struct scheme-number (val) #:transparent)

(define parse-symbol (oneOf "#$%&|*+-/:<=>?@^_~!"))

;; Returns a character literal based on the character
;; escape sequence in a string (such as \t or \n)
(define parse-escaped-char
  (parser-compose
   (char #\\)
   (choice
    (list
     ;;TODO - refactor with a zip
     (>> (char #\b) (return #\backspace))
     (>> (char #\n) (return #\newline))
     (>> (char #\f) (return #\page))
     (>> (char #\t) (return #\tab))
     (>> (char #\r) (return #\return))
     (>> (char #\v) (return #\vtab))
     (>> (char #\\) (return #\\))
     (>> (char #\") (return #\"))))))

;; TODO - this is a bit messed for consuming \"
;; Parses a string (characters delimited by double-quotation marks)
(define parse-string
  (parser-compose
   (chars <- (between
	      (char #\")
	      (char #\")
	      (many (<or>
		     parse-escaped-char
		     (noneOf "\"\\\t\b\n\f\r\v")))))
   (return (scheme-string (list->string chars)))))

;; Creates a parser used to handle base literals
(define (base-parser prefix-char base-parser radix)
  (parser-compose
   (char prefix-char)
   (result <- base-parser)
   (return (scheme-number
			  (string->number (list->string result) radix)))))

;; Parses binary and returns scheme-number
(define parse-base2 (base-parser #\b (many1 (<or> (char #\0) (char #\1))) 2))

;; Parses octal and returns scheme-number
(define parse-base8 (base-parser #\o (many1 (oneOf "01234567")) 8))

;; Parses hexadecimal and returns scheme-number
(define parse-base16 (base-parser #\x (many1 $hexDigit) 16))

;; Returns a scheme-char struct; it handles digits, both lower-and-uppercase
;; chars, symbols, and named-chars such as #\newline
(define parse-char
  (parser-compose
   (char #\\)
   (result <- (<or>
	       (>> (try (string "backspace")) (return #\backspace))
	       (>> (char #\b) (return #\b))
	       (>> (try (string "vtab")) (return #\vtab))
	       (>> (char #\v) (return #\v))
	       (>> (try (string "newline")) (return #\newline))
	       (>> (char #\n) (return #\n))
	       (>> (try (string "page")) (return #\page))
	       (>> (char #\p) (return #\p))
	       (>> (try (string "tab")) (return #\tab))
	       (>> (char #\t) (return #\t))
	       (>> (try (string "return")) (return #\return))
	       (>> (char #\r) (return #\r))
	       (>> (try (string "vtab")) (return #\vtab))
	       (>> (char #\v) (return #\v))
	       $digit
	       $letter
	       parse-symbol
	       ;; delimiters not included in parse-symbol
	       (char #\\)
	       (char #\()
	       (char #\))
	       (char #\])
	       (char #\[)
	       (char #\{)
	       (char #\})))
   (return (scheme-char result))))

;; Returns scheme-bool whose value is either #t or #f
(define parse-boolean
  (parser-compose
   (result <- (<or> (char #\t) (char #\f)))
   (return (scheme-bool (if (equal? result #\t) #t #f)))))

;; Returns scheme-bool, scheme-char, or schemeb-number depending
;; on the liternal notifation (prefix such as #x,#o, #t, #\, etc)
(define parse-literal
  (parser-compose
   (char #\#)
   (result <- (<or> parse-boolean
		    parse-char
		    parse-base2
		    parse-base8
		    parse-base16))
   ;; this blocks extra chars after, say, a boolean, like #tf
   (<or>
    (lookAhead $eof)
    (lookAhead $space)
    (lookAhead $eol))
   (return result)))

;; Returns a scheme-number (handles only integers)
(define parse-number
  (parser-compose
   (digits <- (many1 $digit))
   (return (scheme-number
	    (string->number (list->string digits))))))

;; Returns a scheme-atom, which is any 'value' that does not
;; evaluate to itself (not like number, bools, etc). Another
(define parse-atom
  (<or>
   parse-literal
   (parser-compose
    (first <- (<or> $letter parse-symbol))
    (rest <- (many (<or> $letter $digit parse-symbol)))
    (return
     (scheme-atom (list->string (append (list first) rest)))))))

;; Returns a scheme-atom when it encounters the quote prefix
(define parse-quoted
  (parser-compose
   (char #\')
   (result <- parse-expression)
   (return (list (scheme-atom "quote") result))))

;; Parses a scheme expression, handling atoms in list and outside of
;; lists
(define parse-expression
  (<or>
   (>> (char #\;) (return ""))
   parse-atom
   parse-string
   parse-number
   parse-quoted
   (parser-compose
    (char #\()
    (result <- (<or> (try parse-list) parse-dotted-list))
    (char #\))
    (return result))))

;; Parses a dotted list; (a b . c), return a cons'd list
(define parse-dotted-list
  (parser-compose
   (head <- (endBy parse-expression $spaces))
   (tail <- (>> (>> (char #\.)  $spaces) parse-expression))
   (return (cons head tail))))

;; Parser a list
(define parse-list
  (sepBy parse-expression $spaces))

(module+
  test
  (require rackunit)

  (define (parsack-parse string)
    (parse parse-expression string))

  (define (check-parsed? string expected)
    (test-equal?
     (string-append "parsing: " string)
     (Ok-parsed (force (Consumed-reply (parsack-parse string))))
     expected))

  (define-syntax (check-parse-exn stx)
    (syntax-case stx ()
      [(_ string)
       (syntax/loc stx
         (test-exn
          (string-append "parsing: " string)
          exn:fail:parsack?
          (lambda () (parsack-parse string))))]))

  (test-case
   "boolean"
   (check-parsed? "#t" (scheme-bool #t))
   (check-parsed? "#f" (scheme-bool #f))
   (check-parse-exn "#tf")
   (check-parse-exn "#ft"))

  (test-case
   "char-literals"
   (check-parsed? "#\\g" (scheme-char #\g))
   (check-parsed? "#\\a" (scheme-char #\a))
   (check-parsed? "#\\1" (scheme-char #\1))
   (check-parsed? "#\\\\" (scheme-char #\\))
   (check-parsed? "#\\)" (scheme-char #\)))
   (check-parsed? "#\\#" (scheme-char #\#))

   ;; uppercase
   (check-parsed? "#\\A" (scheme-char #\A))

   ;; chars that can get confused by the named-chars
   ;; e.g. #\backspace
   (check-parsed? "#\\b" (scheme-char #\b))
   (check-parsed? "#\\n" (scheme-char #\n))
   (check-parsed? "#\\p" (scheme-char #\p))
   (check-parsed? "#\\t" (scheme-char #\t))
   (check-parsed? "#\\r" (scheme-char #\r))
   (check-parsed? "#\\v" (scheme-char #\v))

   ;; special chars
   (check-parsed? "#\\backspace" (scheme-char #\backspace))
   (check-parsed? "#\\newline" (scheme-char #\newline))
   (check-parsed? "#\\page" (scheme-char #\page))
   (check-parsed? "#\\tab" (scheme-char #\tab))
   (check-parsed? "#\\return" (scheme-char #\return))
   (check-parsed? "#\\vtab" (scheme-char #\vtab))

   ;; should fail because these are case-sensitive
   (check-parse-exn "#\\BACKSPACE")
   (check-parse-exn "#\\NEWLINE")
   (check-parse-exn "#\\PAGE")
   (check-parse-exn "#\\TAB")
   (check-parse-exn "#\\RETURN")
   (check-parse-exn "#\\VTAB"))

  (test-case
   "strings"

   (check-parsed? "\"a\"" (scheme-string "a"))

   (check-parsed? "\"hello\"" (scheme-string "hello"))

   (check-parsed? "\"\\t\"" (scheme-string "\t"))
   (check-parse-exn "\"\t\"")

   (check-parsed? "\"\\r\"" (scheme-string "\r"))
   (check-parse-exn "\"\r\"")

   (check-parsed? "\"\\b\"" (scheme-string "\b"))
   (check-parse-exn "\"\b\"")

   (check-parsed? "\"\\n\"" (scheme-string "\n"))
   (check-parse-exn "\"\n\"")

   (check-parsed? "\"\\v\"" (scheme-string "\v"))
   (check-parse-exn "\"\v\"")

   (check-parsed? "\"\\f\"" (scheme-string "\f"))
   (check-parse-exn "\"\f\"")

   (check-parsed? "\"\\f\"" (scheme-string "\f"))
   (check-parse-exn "\"\f\"")

   (check-parsed? "\"\\\\\"" (scheme-string "\\"))
   (check-parse-exn "\"\\\"")

   (check-parsed? "\"\\\"\"" (scheme-string "\""))
   #;(check-parse-exn "\"\"\"")
   (let ([in (open-input-string "\"\"\"")])
     (match (Consumed-reply (parsack-parse in))
       [(Ok consumed)
        (check-equal? consumed (scheme-string ""))
        (check-equal? (port->string in) "\"")])))

  (test-case
   "numbers"

   (test-case
    "base 10"
    (check-parsed? "1" (scheme-number 1))
    (check-parsed? "2" (scheme-number 2))
    (check-parsed? "3" (scheme-number 3))
    (check-parsed? "4" (scheme-number 4))
    (check-parsed? "5" (scheme-number 5))
    (check-parsed? "6" (scheme-number 6))
    (check-parsed? "7" (scheme-number 7))
    (check-parsed? "8" (scheme-number 8))
    (check-parsed? "9" (scheme-number 9))
    (check-parsed? "10" (scheme-number 10))
    (check-parsed? "0" (scheme-number 0)))

   (test-case
    "binary"
    (check-parsed? "#b1110" (scheme-number 14))
    (check-parse-exn "#b")
    (check-parse-exn "#b11g10")
    (check-parse-exn "#b11()10")))

  (test-case
   "octal"
   (check-parsed? "#o1237" (scheme-number 671))
   (check-parse-exn "#o")
   (check-parse-exn "#obaXY"))

  (test-case
   "hexadecimal"
   (check-parsed? "#xfff" (scheme-number 4095))
   (check-parse-exn "#x")
   (check-parsed? "#xab8cef102735469" (scheme-number 772594870617592937))
   (check-parse-exn "#xbaXY"))

  (test-case
   "list"
   (check-equal?
    (Ok-parsed (force (Consumed-reply (parsack-parse "()"))))
    (list)))

  (test-case
   "dotted-list"
   (check-equal?
    (Ok-parsed (force (Consumed-reply (parsack-parse "(a b . c)"))))
    (cons (list (scheme-atom "a") (scheme-atom "b")) (scheme-atom "c")))
   (check-equal?
    (Ok-parsed (force (Consumed-reply (parsack-parse "(a . b)"))))
    (cons (list (scheme-atom "a")) (scheme-atom "b"))))

  (test-case
   "quoted"
   (test-equal?
    "parsed quoted"
    (Ok-parsed (force (Consumed-reply (parsack-parse "'()"))))
    (list (scheme-atom "quote") '())))

  (test-case
   "code comments"
   (check-parsed? ";" "")
   (check-parsed? ";;" "")))
