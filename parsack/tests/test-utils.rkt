#lang racket
(require "../parsack.rkt")
(require rackunit)
(provide (all-defined-out))

(define (force-Consumed res)
  (if (Consumed? res)
      (struct-copy Consumed res [reply (force (Consumed-reply res))])
      res))
(define-syntax (check-parsing stx) 
  (syntax-case stx ()
    [(_ (p input-str) parsed rst)
     #`(let ([in (open-input-string input-str)])
         (match (force-Consumed (parse p in))
         [(Consumed (Ok consumed))
          (if (list? consumed) ; if not list, then it's a single char
              #,(syntax/loc #'parsed 
                  (check-equal? consumed (string->list parsed)
                                "parsing result does not match expected"))
              (begin #,(syntax/loc #'parsed 
                         (check-equal? (length (string->list parsed)) 1))
                     #,(syntax/loc #'parsed
                         (check-equal? consumed (car (string->list parsed))))))
          #,(syntax/loc #'rst (check-equal? (port->string in) rst
                                            "remaining input does't not match expected"))]
           [(Empty (Ok res))
            (if (list? res) ; if not list, then it's a single char
                #,(syntax/loc #'parsed 
                    (check-equal? res (string->list parsed)
                                  "parsing result does not match expected"))
                (begin #,(syntax/loc #'parsed 
                           (check-equal? (length (string->list parsed)) 1))
                       #,(syntax/loc #'parsed
                           (check-equal? res (car (string->list parsed))))))
            #,(syntax/loc #'rst (check-equal? (port->string in) rst
                                              "remaining input does't not match expected"))]))]))
(define-syntax (check-parsings stx)
  (syntax-case stx ()
    [(_ (p input-str) parsed ... rst)
     #`(let ([in (open-input-string input-str)])
         (match (force-Consumed (parse p in))
         [(Consumed (Ok consumed))
          #,(syntax/loc (car (syntax->list #'(parsed ...)))
              (check-equal? consumed (list (string->list parsed) ...)))
          #,(syntax/loc #'rst (check-equal? (port->string in) rst
                                            "remaining input does't not match expected"))]))]))
(define-syntax (check-line-parsings stx)
  (syntax-case stx ()
    [(_ (p input-str) (x ...) ... rst)
     #`(let ([in (open-input-string input-str)])
         (match (force-Consumed (parse p in))
         [(Consumed (Ok consumed))
          #,(syntax/loc (car (syntax->list (car (syntax->list #'((x ...) ...)))))
              (check-equal? consumed (list (list (string->list x) ...) ...)))
          #,(syntax/loc #'rst (check-equal? (port->string in) rst
                                            "remaining input does't not match expected"))]))]))
(define-syntax (check-empty-parsing stx)
  (syntax-case stx ()
    [(_ e rst) (syntax/loc stx (check-parsing e "" rst) #;(check-empty-parsing e "" rst))]
    [(_ e parsed rst)
     (syntax/loc stx (check-parsing e parsed rst))
     #;#`(let ([in (open-input-string input-str)])
         (match (p in) #;(force-Consumed e)
         [result #;(Empty (Ok result (State remaining pos _) (Msg pos msg exp)))
          (if (list? result)
              #,(syntax/loc #'parsed (check-equal? result (string->list parsed)))
              (begin #,(syntax/loc #'parsed 
                         (check-equal? (length (string->list parsed)) 1))
                     #,(syntax/loc #'parsed 
                         (check-equal? result (car (string->list parsed))))))
          #,(syntax/loc #'rst (check-equal? remaining rst))]))]))
(define-syntax (check-parse-error stx)
  (syntax-case stx ()
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
            (force-Consumed (with-input-from-string input-str (curry parse p))) #;(force-Consumed e)))))]))
#;(define-syntax (check-partial-parse-error stx)
  (syntax-case stx ()
    [(_ e) (syntax/loc stx (check-partial-parse-error e ""))]
    [(_ e msg)
     (quasisyntax/loc stx
       (check-exn exn:fail:parsack? 
         (thunk
          (with-handlers 
            ([exn:fail:parsack? (λ (x) 
                                  #,(syntax/loc #'msg (check-equal? (exn-message x) msg
                                                                    "err msg doesn't match expected"))
                                  (raise x))])
            (force-Consumed e)))))]))

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
