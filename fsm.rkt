#lang racket
(require racket/hash)
(require plot)
(plot-new-window? #t)

(provide (all-defined-out))
;;todo
;; markov automaton

(define ACTION# 2)
(define ACTIONS (list 'C 'D))

(struct automaton (payoff initial cc cd dc dd) #:transparent)

(define C (automaton 0 1 1  1 1 1))
(define D (automaton 0 0 0  0 0 0))
(define C1 (automaton 0 0.1  0.1 0.1 0.1 0.1))
(define C2 (automaton 0 0.2  0.2 0.2 0.2 0.2))
(define C3 (automaton 0 0.3  0.3 0.3 0.3 0.3))
(define C4 (automaton 0 0.4  0.4 0.4 0.4 0.4))
(define C5 (automaton 0 0.5  0.5 0.5 0.5 0.5))
(define C6 (automaton 0 0.6  0.6 0.6 0.6 0.6))
(define C7 (automaton 0 0.7  0.7 0.7 0.7 0.7))
(define C8 (automaton 0 0.8  0.8 0.8 0.8 0.8))
(define C9 (automaton 0 0.9  0.9 0.9 0.9 0.9))

(define (randomise probability)
  (define r (random))
  (for/last
      ([s (in-naturals)]
       [p (in-list (list probability 1))]
       #:final (< r p)) s))

(define PAYOFF-TABLE
  (list
   (list (cons 3 3) (cons 0 4))
   (list (cons 4 0) (cons 1 1))))


(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))

(define (round4 n)
  (/ (round (* n 10000)) 10000))

(define (make-random-automaton)
  (automaton 0 (round4 (random))
             (round4 (random))
             (round4 (random))
             (round4 (random))
             (round4 (random))))



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
  (values results
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


(define (mutate auto)
  (match-define (automaton pay initial cc cd dc dd) auto)
  (define r (random 5))
  (cond
    [(zero? r) (automaton pay (+ initial 0.1) cc cd dc dd)]
    [(= r 1) (automaton pay initial (+ cc 0.1) cd dc dd)]
    [(= r 2) (automaton pay initial cc (+ cd 0.1) dc dd)]
    [(= r 3) (automaton pay initial cc cd (+ dc 0.1) dd)]
    [(= r 4) (automaton pay initial cc cd dc (+ dd 0.1))]))



(define (build-random-population n)
  (build-vector n (lambda (_) (make-random-automaton))))

(define (population-payoffs population)
  (for/list
      ([auto population])
    (automaton-payoff auto)))
(define (match-population population rounds delta)
  ;(population-reset population)
  (for
      ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define auto1 (vector-ref population i))
    (define auto2 (vector-ref population (+ i 1)))
    (define-values (a1 a2)
      (interact auto1 auto2 rounds delta))
    (vector-set! population i a1)
    (vector-set! population (+ i 1) a2))
  population)

(define (sum l)
  (apply + l))

(define (payoff->fitness population)
  (define payoffs (population-payoffs population))
  (define total (sum payoffs))
  (for/list ([p (in-list payoffs)])
    (/ p total)))

(define (shuffle-vector vec)
  (define lst (vector->list vec))
  (define l2 (shuffle lst))
  (list->vector l2))

(define (accumulate-fitness probabilities)
  (let relative->absolute
      ([payoffs probabilities] [so-far #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons nxt (relative->absolute (rest payoffs) nxt))])))
(define (randomise-s probabilities speed)
  (define fitness (accumulate-fitness probabilities))
  (for/list ([n (in-range speed)])
    (define r (random))
    (for/last ([p (in-naturals)]
               [% (in-list fitness)]
               #:final (< r %)) p)))

(define (regenerate population rate)
  (define probabilities (payoff->fitness population))
  (define substitutes (randomise-s probabilities rate))
  (for ([i (in-range rate)]
        [auto (in-list substitutes)])
    (vector-set! population i
                 (vector-ref population auto)))
  (shuffle-vector population))

(define (reset a)
  (match-define (automaton pay initial cc cd dc dd) a)
  (automaton 0 initial cc cd dc dd))

(define (population-reset population)
  (for ([auto population]
        [i (in-naturals)])
    (vector-set! population i (reset auto))))

(define (average lst)
  (exact->inexact (/ (sum lst) (length lst))))

(define (evolve population cycles speed rounds delta)
  (cond
    [(zero? cycles) (list population)]
    [else
     (define p2 (match-population population rounds delta))
     (define pp (population-payoffs p2))
     (define p3 (regenerate p2 speed))
     (define auto (vector-ref p3 0))
     (define p4 (vector-set! p3 0 (mutate auto)))
     (cons (average pp)
           (evolve (vector-map reset p3) (- cycles 1)
                   speed rounds delta))]))

(define (population-mean->lines data)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors))

(define (compound d r)
  (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))


(define (plot-mean data delta rounds)
  (define reward (* 3 (compound delta rounds)))
  (define punishment (* 1 (compound delta rounds)))
  (define reward-line
    (function (lambda (x) reward) #:color "blue"))
  (define punishment-line
    (function (lambda (x) punishment) #:color "red"))
  (plot (list reward-line punishment-line
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 reward) #:width 1200 #:height 800))


(define (scan population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   p))
