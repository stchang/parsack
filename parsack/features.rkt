#lang racket


(require feature-profile
         feature-profile/plug-in-lib
         (only-in profile/render-text render)
         (only-in profile/analyzer    analyze-samples))

(provide (all-defined-out))

(define parsack-features
  (list
   (feature "Parsack Backtracking" 'feature-profile:parsack-backtracking
            values ;; No sorting
            ;; Idea from https://github.com/stamourv/marketplace
            ;; commit c3574966bc
            (λ (f-p)
              (define items ; Non Terminals
                (for/fold ([l '()])
                          ([i (feature-report-core-samples f-p)])
                  (append i l)))
              (define nt-b ; Non Terminal -> Backtracking Count
                (for/fold ([table (hash)])
                          ([i items])
                  (match i
                    [`(,or ,bt ,id) (hash-update table id (λ (x) (max bt x)) bt)]
                    [else           table])))
              (define intern (make-interner))
              (define post-processed
                (for/list ([c-s (feature-report-core-samples f-p)]
                           [p-s (cdr (feature-report-raw-samples f-p))])
                  (define processed
                      (for/list ([i c-s])
                        (match i
                          [`(,or ,bt ,id) #:when (bt . < . (hash-ref nt-b id))
                           `(bt-<or> ,bt ,id)]
                          [else i])))
                  (list* (car p-s) (cadr p-s) ; thread id and timestamp
                         (for/list ([v processed])
                           (intern (cons v #f))))))
              ;; Call edge profiler
              (newline) (newline) (displayln "Parsack Backtracking")
              (render (analyze-samples (cons (feature-report-total-time f-p) post-processed)))))))
