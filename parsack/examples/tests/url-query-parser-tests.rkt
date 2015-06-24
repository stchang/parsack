#lang racket
(require "../../parsack.rkt")
(require "../../tests/test-utils.rkt")
(require "../url-query-parser.rkt")
(require rackunit)
(require (for-syntax syntax/parse))

(define-syntax (check-query-parse stx)
  (syntax-parse stx
    [(_ p (~seq x y) ...)
     #'(check-parse p (list (cons (string->list x) (string->list y)) ...))]))
                   
(check-query-parse (parse $p_query "foo=bar&a%21=b+c")
                   "foo" "bar" "a!" "b c")