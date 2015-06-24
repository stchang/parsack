#lang racket
(require (for-syntax syntax/parse))
(require "../parsack.rkt")
(require rackunit)
(require/expose "../parsack.rkt" (format-exp format-pos Pos))
(provide (all-defined-out))

(define-syntax (check-parse stx)
  (syntax-parse stx
    [(_ (_ p inp) expected)
     (syntax/loc #'p
       (check-equal? (parse-result p inp) expected))]))

(define-syntax (check-parsing stx) 
  (syntax-parse stx
    [(_ e parsed:str rst:str) ; explode expected parsing
     #'(check-parsing e (string->list parsed) rst)]
    [(_ (p input-str) parsed rst:str) ; parsed is either char or (nested) list(s) of chars
     #`(let* ([in (open-input-string input-str)]
              [result (parse-result p in)]
              [remaining (port->string in)])
         (cond [(list? result) ; result is single char or (nested) list(s) of chars
                #,(syntax/loc #'parsed 
                    (check-equal? result parsed
                                  "parsing result does not match expected"))]
               [else #,(syntax/loc #'parsed 
                         (check-equal? (length parsed) 1))
                     #,(syntax/loc #'parsed
                         (check-equal? result (car parsed)))])
         #,(syntax/loc #'rst
             (check-equal? remaining rst
                           "remaining input does't not match expected")))]
    [x #:when (printf "no match: ~a\n" (syntax->datum #'x)) #'(void)]))

(define-syntax (check-parsings stx)
  (syntax-parse stx
    [(_ e parsed ... rst)
     #'(check-parsing e (list (string->list parsed) ...) rst)]))

(define-syntax (check-line-parsings stx)
  (syntax-parse stx
    [(_ e (x ...) ... rst)
     #'(check-parsing e (list (list (string->list x) ...) ...) rst)]))

(define-syntax (check-empty-parsing stx)
  (syntax-parse stx
    [(_ e rst) (syntax/loc stx (check-parsing e "" rst))]
    [(_ e parsed rst) (syntax/loc stx (check-parsing e parsed rst))]))

(define-syntax (check-parse-error stx)
  (syntax-parse stx
    [(_ e) (syntax/loc stx (check-parse-error e ""))]
    [(_ (p input-str) msg)
     (quasisyntax/loc stx
       (check-exn exn:fail:parsack? 
         (thunk
          (with-handlers 
            ([exn:fail:parsack?
              (λ (x) 
                #,(syntax/loc #'msg (check-equal? (exn-message x) msg
                                                  "err msg doesn't match expected"))
                (raise x))])
            (with-input-from-string input-str (curry parse p))))))]))


(define-syntax-rule (do-parse (p inp)) (parse p inp))

;; FIXME:
;; test seemingly passes when this is used with check-parse-error and
;; strs is not a list -- because error handler is not fine-grain enough
;;
;; Supply line, col, and pos as 1-based (as they appear in the errorw
;; message).
(define (fmt-err-msg line col pos str strs #:extra [extra #f])
  (define tmp (if extra
                  (string-append extra ": " (format-exp strs)) 
                  (format-exp strs)))
  (format "parse ERROR: at ~a\nunexpected: ~s\n  expected: ~s"
          (format-pos (Pos line col pos))
          str
          tmp))
