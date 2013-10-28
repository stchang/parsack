#lang racket
(require "../parsack.rkt")
(require rackunit)
(provide (all-defined-out))

(define-syntax-rule (force-Consumed (p inp))
  (let ([res (parse p inp)])
    (if (Consumed? res)
        (struct-copy Consumed res [reply (force (Consumed-reply res))])
        res)))
(define-syntax-rule (check-parsing e parsed rst)
  (match (force-Consumed e)
    [(Consumed (Ok consumed (State remaining pos) (Msg pos2 msg exp)))
     (if (list? consumed)
         (check-equal? consumed (string->list parsed))
         (check-equal? consumed (car (string->list parsed))))
     (check-equal? remaining rst)]))
(define-syntax-rule (check-parsings e parsed ... rst)
  (match (force-Consumed e)
    [(Consumed (Ok consumed (State remaining pos) (Msg pos msg exp)))
     (check-equal? consumed (list (string->list parsed) ...))
     (check-equal? remaining rst)]))
(define-syntax-rule (check-line-parsings e (x ...) ... rst)
  (match (force-Consumed e)
    [(Consumed (Ok consumed (State remaining pos) (Msg pos msg exp)))
     (check-equal? consumed (list (list (string->list x) ...) ...))
     (check-equal? remaining rst)]))
(define-syntax-rule (check-empty-parsing e rst)
  (match (force-Consumed e)
    [(Empty (Ok consumed (State remaining pos) (Msg pos msg exp)))
     (check-true (null? consumed))
     (check-equal? remaining rst)]))
(define-syntax check-parse-error
  (syntax-rules ()
    [(_ e) (check-parse-error e "")]
    [(_ e msg)
     (check-exn exn:fail? 
       (thunk
        (with-handlers ([exn:fail? (λ (x) (check-equal? (exn-message x) msg)
                                     (raise x))])
          (force-Consumed e))))]))
(define-syntax check-partial-parse-error
  (syntax-rules ()
    [(_ e) (check-partial-parse-error e "")]
    [(_ e msg)
     (check-exn exn:fail? 
       (thunk
        (with-handlers ([exn:fail? (λ (x) (check-equal? (exn-message x) msg)
                                     (raise x))])
          (force-Consumed e))))])
  #;(match (force-Consumed e)
    [(Consumed (Error (Msg pos msg exp)))
     (check-true true)]))

(define-syntax-rule (do-parse (p inp)) (parse p inp))

;; FIXME:
;; test seemingly passes when this is used with check-parse-error and
;; strs is not a list -- because error handler is not fine-grain enough
(define (fmt-err-msg pos str strs #:extra [extra #f]) 
  (define tmp (if extra
                  (string-append extra ": " (format-exp strs)) 
                  (format-exp strs)))
  (format "parse-error: at pos ~a\nunexpected: ~s\n  expected: ~s" pos str tmp))