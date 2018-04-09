#lang racket
(provide (all-defined-out))

(define (randomise probability)
  (define r (random))
  (for/last
      ([s (in-naturals)]
       [p (in-list (list probability 1))]
       #:final (< r p)) s))

(define ACTION# 2)
(define ACTIONS (list 'C 'D))

(struct automaton (payoff initial cc cd dc dd) #:transparent)

(define C (automaton 0 1 1  1 1 1))
(define D (automaton 0 0 0  0 0 0))
(define TFT (automaton 0 1  1 0 1 0))
(define GT (automaton 0 1  1 0 0 0))
(define P (automaton 0 0  0 1 0 1))

(define C1 (automaton 0 0.1  0.1 0.1 0.1 0.1))
(define C2 (automaton 0 0.2  0.2 0.2 0.2 0.2))
(define C3 (automaton 0 0.3  0.3 0.3 0.3 0.3))
(define C4 (automaton 0 0.4  0.4 0.4 0.4 0.4))
(define C5 (automaton 0 0.5  0.5 0.5 0.5 0.5))
(define C6 (automaton 0 0.6  0.6 0.6 0.6 0.6))
(define C7 (automaton 0 0.7  0.7 0.7 0.7 0.7))
(define C8 (automaton 0 0.8  0.8 0.8 0.8 0.8))
(define C9 (automaton 0 0.9  0.9 0.9 0.9 0.9))

(define PAYOFF-TABLE
  (list
   (list (cons 3 3) (cons 0 4))
   (list (cons 4 0) (cons 1 1))))


(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))



(define (round1 n)
  (/ (round (* n 100)) 100))

(define (round-auto auto)
(match-define (automaton pay init cc cd dc dd) auto)
(automaton pay (round1 init)
	(round1 cc)
	(round1 cd)
(round1 dc)
(round1 dd)))

(define (make-random-automaton)
  (define auto  (automaton 0 (random)
                           (random)
                           (random)
                           (random)
                           (random)))
  (round-auto auto))


(define (interact-d au1 au2 rounds delta)
  (match-define (automaton pay1 init1 cc1 cd1 dc1 dd1) au1)
  (match-define (automaton pay2 init2 cc2 cd2 dc2 dd2) au2)
  (define (what-next? action1 action2)
    (if
     (zero? action1)
     (if (zero? action2) (cons cc1 cc2) (cons cd1 cd2))
     (if (zero? action2) (cons dc1 dc2) (cons dd1 dd2))))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-range rounds)])
      ; #:final (> (random) delta)
      (match-define (list a1 a2)
        (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (what-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (reverse results)
   (automaton p1 init1 cc1 cd1 dc1 dd1)
   (automaton p2 init2 cc2 cd2 dc2 dd2)))

(define (interact-ds au1 au2 rounds delta)
  (match-define (automaton pay1 init1 cc1 cd1 dc1 dd1) au1)
  (match-define (automaton pay2 init2 cc2 cd2 dc2 dd2) au2)
  (define (what-next? action1 action2)
    (if
     (zero? action1)
     (if (zero? action2) (cons cc1 cc2) (cons cd1 cd2))
     (if (zero? action2) (cons dc1 dc2) (cons dd1 dd2))))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-range rounds)])
      ; #:final (> (random) delta)
      (match-define (list a1 a2)
        (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (what-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (take (reverse results) 20)
   (automaton p1 init1 cc1 cd1 dc1 dd1)
   (automaton p2 init2 cc2 cd2 dc2 dd2)))


(define (interact au1 au2 rounds delta)
  (match-define (automaton pay1 init1 cc1 cd1 dc1 dd1) au1)
  (match-define (automaton pay2 init2 cc2 cd2 dc2 dd2) au2)
  (define (what-next? action1 action2)
    (if
     (zero? action1)
     (if (zero? action2) (cons cc1 cc2) (cons cd1 cd2))
     (if (zero? action2) (cons dc1 dc2) (cons dd1 dd2))))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-range rounds)])
      ; #:final (> (random) delta)
      (match-define (list a1 a2)
        (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (what-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 (expt delta _)))
              (+ payoff2 (* pa2 (expt delta _)))
              (cons (cons pa1 pa2) round-results)
              )))
  (values
   (automaton p1 init1 cc1 cd1 dc1 dd1)
   (automaton p2 init2 cc2 cd2 dc2 dd2)))

(define (random-decimal prob)
  (define n (inexact->exact (round (* prob 100))))
  (if (zero? prob)
      0
      (round1 (exact->inexact (/ (random n) 100)))))
(define (mutate-b prob)
;;  (print (number->string prob))
  (define r (random 2))
;;  (print (number->string r))
  (define decrease (random-decimal prob))
;;  (print (number->string decrease))
  (define increase (random-decimal (round1 (- 1 prob))))
;;  (print (number->string increase))
  (if (zero? r)
      (+ prob increase)
      (- prob decrease)))

(define (mutate auto)
  (match-define (automaton pay initial cc cd dc dd) auto)
  (define r (random 5))
  (cond
   [(zero? r) (automaton pay (mutate-b initial) cc cd dc dd)]
   [(= r 1) (automaton pay initial (mutate-b cc) cd dc dd)]
   [(= r 2) (automaton pay initial cc (mutate-b cd) dc dd)]
   [(= r 3) (automaton pay initial cc cd (mutate-b dc) dd)]
   [(= r 4) (automaton pay initial cc cd dc (mutate-b dd))]))
