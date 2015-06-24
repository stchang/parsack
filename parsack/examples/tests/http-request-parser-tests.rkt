#lang racket
(require "../../parsack.rkt")
(require "../../tests/test-utils.rkt")
(require "../http-request-parser.rkt")
(require rackunit)

;; examples from: 
;; http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
;; http://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol
;; http://search.cpan.org/~gaas/HTTP-Message-6.06/lib/HTTP/Request/Common.pm

;; no terminating \n
(check-exn exn:fail:parsack? (thunk (parse $p_request "GET /pub/WWW/TheProject.html HTTP/1.1")))

(check-parse (parse $p_request "GET /pub/WWW/TheProject.html HTTP/1.1\n\n")
             (HttpRequest 'GET "pub/WWW/TheProject.html" null #f))

(check-parse (parse $fieldName "Host") "Host")

(check-parse (parse $header "Host: www.w3.org\n") (cons "Host" "www.w3.org"))

(check-parse (parse $p_request "GET /pub/WWW/TheProject.html HTTP/1.1\nHost: www.w3.org\n\n")
             (HttpRequest 'GET "pub/WWW/TheProject.html" '(("Host" . "www.w3.org")) #f))

(check-parse (parse $p_request "GET /index.html HTTP/1.1\nHost: www.example.com\n\n")
             (HttpRequest 'GET "index.html" '(("Host" . "www.example.com")) #f))

(check-parse (parse 
              $p_request 
              "GET /index.html HTTP/1.1\nHost: www.example.com\nHost: www.w3.org\n\n")
              (HttpRequest 'GET "index.html" '(("Host" . "www.example.com") 
                                               ("Host" . "www.w3.org")) #f))



(check-parse 
 (parse
  $p_request
  (string-append "POST http://www.perl.org/survey.cgi HTTP/1.1\n"
                 "Content-Length: 66\n"
                 "Content-Type: application/x-www-form-urlencoded\n"
                 "\n"))
 (HttpRequest
   'POST
   "http://www.perl.org/survey.cgi"
   '(("Content-Length" . "66") ("Content-Type" . "application/x-www-form-urlencoded"))
   ""))

(check-parse
 (parse
  $p_request
  (string-append "POST http://www.perl.org/survey.cgi HTTP/1.1\n"
                 "Content-Length: 66\n"
                 "Content-Type: application/x-www-form-urlencoded\n"
                 "\n"
                 "name=Gisle%20Aas&email=gisle%40aas.no&gender=M&born=1964&perc=3%25"))
 (HttpRequest
  'POST
  "http://www.perl.org/survey.cgi"
  '(("Content-Length" . "66") ("Content-Type" . "application/x-www-form-urlencoded"))
  "name=Gisle%20Aas&email=gisle%40aas.no&gender=M&born=1964&perc=3%25"))

(check-parse
 (parse
  $p_request
  (string-append "POST http://www.perl.org/survey.cgi HTTP/1.1\n"
                 "Content-Length: 388\n"
                 "Content-Type: multipart/form-data; boundary=\"6G+f\"\n"
                 "\n"
                 "--6G+f\n"
                 "Content-Disposition: form-data; name=\"name\"\n"
                 "\n"
                 "Gisle Aas\n"
                 "--6G+f\n"
                 "Content-Disposition: form-data; name=\"email\"\n"
                 "\n"
                 "gisle@aas.no\n"
                 "--6G+f\n"
                 "Content-Disposition: form-data; name=\"gender\"\n"
                 "\n"
                 "M\n"
                 "--6G+f\n"
                 "Content-Disposition: form-data; name=\"born\"\n"
                 "\n"
                 "19\n64"
                 "--6G+f\n"
                 "Content-Disposition: form-data; name=\"init\"; filename=\".profile\"\n"
                 "Content-Type: text/plain\n"
                 "\n"
                 "PATH=/local/perl/bin:$PATH\n"
                 "export PATH\n"
                 "\n"
                 "--6G+f--"))
 (HttpRequest
  'POST
  "http://www.perl.org/survey.cgi"
  '(("Content-Length" . "388") ("Content-Type" . "multipart/form-data; boundary=\"6G+f\""))
  "--6G+f\nContent-Disposition: form-data; name=\"name\"\n\nGisle Aas\n--6G+f\nContent-Disposition: form-data; name=\"email\"\n\ngisle@aas.no\n--6G+f\nContent-Disposition: form-data; name=\"gender\"\n\nM\n--6G+f\nContent-Disposition: form-data; name=\"born\"\n\n19\n64--6G+f\nContent-Disposition: form-data; name=\"init\"; filename=\".profile\"\nContent-Type: text/plain\n\nPATH=/local/perl/bin:$PATH\nexport PATH\n\n--6G+f--"))