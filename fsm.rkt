#lang racket
(require racket/hash)
(require plot)
(plot-new-window? #t)

(provide (all-defined-out))

;; todo
;; mutate that makes sense
;; delta into repeated game

(define ACTION# 2)
(define ACTIONS (list 'C 'D))

(struct automaton (payoff probability) #:transparent)

(define C (automaton 0 1))
(define D (automaton 0 0))
(define C1 (automaton 0 0.1))
(define C2 (automaton 0 0.2))
(define C3 (automaton 0 0.3))
(define C4 (automaton 0 0.4))
(define C5 (automaton 0 0.5))
(define C6 (automaton 0 0.6))
(define C7 (automaton 0 0.7))
(define C8 (automaton 0 0.8))
(define C9 (automaton 0 0.9))

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

(define (interact au1 au2)
  (match-define (list p1 p2)
    (list (automaton-probability au1)
          (automaton-probability au2)))
  (match-define (list a1 a2)
    (list (randomise p1)
          (randomise p2)))
  (list
   (list-ref
    (list-ref PAYOFF-TABLE a1)
    a2)
   (cons au1 au2)))

(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))

(define (make-random-automaton)
  (automaton 0 (random)))
  
(define (interact-r au1 au2 rounds)
  (match-define (cons prob1 prob2)
    (cons
     (automaton-probability au1)
     (automaton-probability au2)))
  (define-values (p1 p2)
    (for/fold ([payoff1 (automaton-payoff au1)]
               [payoff2 (automaton-payoff au2)]
               )
              ([_ (in-range rounds)])
      (match-define (list a1 a2)
        (list (randomise prob1) (randomise prob2)))
      (match-define (cons pay1 pay2) (payoff a1 a2))
      (values (+ pay1 payoff1)
              (+ pay2 payoff2))))
  (values
   (automaton p1 prob1)
   (automaton p2 prob2)))


(define (build-random-population n)
  (build-vector n (lambda (_) (make-random-automaton))))

(define (population-payoffs population)
  (for/list
      ([auto population])
    (automaton-payoff auto)))
(define (match-population population rounds)
  ;(population-reset population)
  (for
      ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define auto1 (vector-ref population i))
    (define auto2 (vector-ref population (+ i 1)))
    (define-values (a1 a2)
      (interact-r auto1 auto2 rounds))
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
  (match-define (automaton pay prob) a)
  (automaton 0 prob))

(define (population-reset population)
  (for ([auto population]
        [i (in-naturals)])
    (vector-set! population i (reset auto))))

(define (average lst)
  (exact->inexact (/ (sum lst) (length lst))))

(define (evolve population cycles speed rounds)
  (cond
    [(zero? cycles) (list population)]
    [else
     (define p2 (match-population population rounds))
     (define pp (population-payoffs p2))
     (define p3 (regenerate p2 speed))
     (define p4 (vector-set! p3 0 (make-random-automaton)))
     (cons (average pp)
           (evolve (vector-map reset p3) (- cycles 1)
                   speed rounds))]))

(define (population-mean->lines data)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors))

(define (plot-mean data rounds)
  (define reward (* 3 rounds))
  (define punishment (* 1 rounds))
  (define reward-line
    (function (lambda (x) reward) #:color "blue"))
  (define punishment-line
    (function (lambda (x) punishment) #:color "red"))
  (plot (list reward-line punishment-line
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 reward) #:width 1200 #:height 800))

