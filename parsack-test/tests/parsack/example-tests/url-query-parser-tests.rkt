#lang racket
(require parsack
         tests/parsack/test-utils
         parsack/examples/url-query-parser
         rackunit
         (for-syntax syntax/parse))

(define-syntax (check-query-parse stx)
  (syntax-parse stx
    [(_ p (~seq x y) ...)
     #'(check-parse p (list (cons (string->list x) (string->list y)) ...))]))
                   
(check-query-parse (parse $p_query "foo=bar&a%21=b+c")
                   "foo" "bar" "a!" "b c")
