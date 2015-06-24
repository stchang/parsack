#lang racket/base

(require parsack
         parsack/examples/json-parser
         racket/runtime-path)

(define-runtime-path json-file "json.json")

(displayln "parsack json parser:")
(for ([i 3]) (collect-garbage))
(time (void (with-input-from-file json-file (λ () (parse $p_text)))))

(require json)
(displayln "Racket read-json:")
(for ([i 3]) (collect-garbage))
(time (void (with-input-from-file json-file read-json)))

;; 2015-06-24, with parsack 0.4 (ports)
; output, i7-2700k, 16gb
;$ racket json-perf-test.rkt
;parsack json parser:
;cpu time: 8 real time: 8 gc time: 0
;Racket read-json:
;cpu time: 4 real time: 3 gc time: 0
