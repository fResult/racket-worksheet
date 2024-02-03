#lang typed/racket

(define (rule ch)
  (cond [(equal? ch #\a) "b"]
        [(equal? ch #\b) "ab"]
        [else (error "no rule")]))

(: apply-rule (String -> String))
(define (apply-rule str)
  (string-append* (map rule (string->list str))))

(: length-naive-gen (Number -> Number))
(define (length-naive-gen n)
  (local {
          (: iterate-apply-rule (Number -> String -> String))
          (define (iterate-apply-rule n str)
            (if (= n 0)
                str
                (iterate-apply-rule (- n 1) (apply-rule str))))
          }
    (string-length (iterate-apply-rule n "a"))))

(: fibonacci (Number -> Number))
(define (fibonacci n)
  (cond [(= n 0) 1]
        [(= n 1) 1]
        [else (+ (fibonacci (- n 2)) (fibonacci (- n 1)))]))
