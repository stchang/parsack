#lang racket
(require "parser-with-errors.rkt")
(provide (all-defined-out))

;; A Method is either 'get or 'post
;; An HttpRequest is a
;; (HttpRequest Method String [Listof [Pairof String String]] (String or #f))
(struct HttpRequest (method url headers body) #:transparent)

(define fieldChar
  (<or> letter
        digit
        (oneOf "-_")))

(define fieldName
  (parser-compose
   (x <- letter)
   (xs <- (many fieldChar))
   (return (list->string (cons x xs)))))

(define continuation
  (parser-compose
   (x <- (>> (many1 (oneOf " \t")) (return #\space)))
   (xs <- contents)
   (return (cons x xs))))

(define notEOL (noneOf "\r\n"))
(define crlf
  (<or> (>> (string "\r\n") (return null))
        (>> newline (return null))))

(define contents
  (parser-compose
   (xs <- (parser-compose (x <- (many1 notEOL)) crlf (return x)))
   (ys <- (<or> continuation (return null)))
   (return (list->string (append xs ys)))))

(define header
  (parser-compose
   (x <- fieldName)
   (y <- (parser-compose (char #\:) spaces contents))
   (return (cons x y))))
   
(define p_headers (manyTill header crlf))
  
(define url
  (parser-compose
   (optional (char #\/))
   (x <- (manyTill notEOL (try (parser-compose (x <- (string " HTTP/1."))
                                               (oneOf "01") (return x)))))
   crlf
   (return x)))
(define (q name ctor body)
  (parser-compose
   (m <- (parser-compose
             (string name)
             (char #\space)
             (return ctor)))
   (u <- url)
   (hs <- p_headers)
   (b <- body)
   (return (HttpRequest m (list->string u) hs (and b (list->string b))))))

;; parsers Http Request
(define p_request
  (<or> (q "GET" 'GET (return #f))
        (q "POST" 'POST (many anyChar))))