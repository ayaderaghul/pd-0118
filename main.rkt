#lang racket
(require "auto.rkt")
(require "inout.rkt")
(require plot)
(require racket/hash)
(plot-new-window? #t)
(provide (all-defined-out))
;;todo
;; markov automaton


;; CONFIGURATION
(define N 100)
(define CYCLES 1000)
(define SPEED 10)
(define ROUNDS 100)
(define DELTA .95)
(define MUTATION 1)




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

(define (scan population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   p))



(define (population-mean->lines data)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors))

(define (compound d r)
  (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))


(define (plot-mean data delta rounds)
  (define reward (* 4 (compound delta rounds)))
  (define punishment (* 0 (compound delta rounds)))
  (define reward-line
    (function (lambda (x) reward) #:color "blue"))
  (define punishment-line
    (function (lambda (x) punishment) #:color "red"))
  (plot (list reward-line punishment-line
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 reward) #:width 1200 #:height 800))


(define (sort-population p)
 (sort (hash->list (scan (vector-map reset p)))
       > #:key cdr))


;; MUTATE
(define (mutate-population population rate)
  (for ([i (in-range rate)])
    (vector-set! population i (mutate (vector-ref population i)))))

(define (evolve population cycles speed mutation rounds delta mean-file rank-file)
  (cond
    [(zero? cycles) '()]
    [else
     (define p2 (match-population population rounds delta))
     (define pp (population-payoffs p2))
     (define p3 (regenerate p2 speed))
     (define auto (vector-ref p3 0))
     (mutate-population p3 mutation)
	(and (zero? (modulo cycles 100)) (out-rank cycles (scan p3) rank-file))
     (out-data mean-file (list (list (average pp))))
           (evolve (vector-map reset p3) (- cycles 1)
                   speed mutation rounds delta mean-file rank-file)]))



(define (main)
(collect-garbage)
(define A (build-random-population N))
(time (evolve A CYCLES SPEED ROUNDS DELTA MUTATION "mean" "rank"))
)

