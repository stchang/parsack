#lang racket
(require "../parsack.rkt")
(provide (all-defined-out))

;; A Method is either 'get or 'post
;; An HttpRequest is a
;; (HttpRequest Method String [Listof [Pairof String String]] (String or #f))
(struct HttpRequest (method url headers body) #:transparent)

(define $fieldChar
  (<or> $letter
        $digit
        (oneOf "-_")))

(define-syntax-rule (http-parser-cons x y)
  (parser-seq x y #:combine-with (compose list->string cons)))

(define $fieldName (http-parser-cons $letter (many $fieldChar))
  #;(parser-compose
     (x  <- $letter)
     (xs <- (many $fieldChar))
     (return (list->string (cons x xs)))))

(define $continuation 
  (parser-cons (>> (many1 (oneOf " \t")) (return #\space)) $contents)
  #;(parser-compose
     (x <- (>> (many1 (oneOf " \t")) (return #\space)))
     (xs <- $contents)
     (return (cons x xs))))

(define $notEOL (noneOf "\r\n"))
(define $crlf
  (<or> (>> (string "\r\n") (return null))
        (>> $newline (return null))))

(define $contents
  (parser-seq
   (parser-one (~> (many1 $notEOL)) $crlf)
   (<or> $continuation (return null))
   #:combine-with (compose list->string append))
  #;(parser-compose
     (xs <- (parser-one (~> (many1 $notEOL)) $crlf))
     (ys <- (<or> $continuation (return null)))
     (return (list->string (append xs ys)))))

(define $header (parser-cons $fieldName (parser-compose (char #\:) $spaces $contents))
  #;(parser-compose
   (x <- $fieldName)
   (y <- (parser-compose (char #\:) $spaces $contents))
   (return (cons x y))))
   
(define $p_headers (manyTill $header $crlf))
  
(define $url
  (parser-one
   (optional (char #\/))
   (~> (manyTill $notEOL (try (parser-one (~> (string " HTTP/1.")) (oneOf "01")))))
   $crlf)
  #;(parser-compose
     (optional (char #\/))
     (x <- (manyTill $notEOL (try (parser-compose (x <- (string " HTTP/1."))
                                                  (oneOf "01") (return x)))))
     $crlf
     (return x)))
(define (q name ctor body)
  (parser-compose
   (m <- (parser-compose
          (string name)
          (char #\space)
          (return ctor)))
   (u <- $url)
   (hs <- $p_headers)
   (b <- body)
   (return (HttpRequest m (list->string u) hs (list->string b)))))

;; parsers Http Request
(define $p_request
  (<or> (q "GET" 'GET (return null)) ; change #f to null, can't parse to #f
        (q "POST" 'POST (many $anyChar))))