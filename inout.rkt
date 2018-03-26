#lang racket

(require "csv.rkt")

(provide (all-defined-out))

;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-rank cycles rankings rank-file)
(out-data rank-file (list (list cycles)))
(out-data rank-file (map list (take (sort (hash->list rankings) > #:key cdr) 4))))
