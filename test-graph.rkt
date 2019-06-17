#lang racket

(require "graph.rkt")
(require rackunit)
(require rackunit/text-ui)

(define graph-tests
  (test-suite
   "Tests for dijkstra [graph.rkt]"
 
   (test-case
    "Simple grid-like graph"
    (let ([g (hash
              'A (hash 'C 1 'B 1 'F 1)
              'B (hash 'A 1 'C 1 'D 1)
              'C (hash 'A 1 'B 1 'D 1)
              'D (hash 'B 1 'E 1)
              'E (hash 'F 1 'D 1)
              'F (hash 'A 1 'E 1))])
      (check-equal? (unwind-path (dijkstra g #:start 'A #:end 'E) 'A 'E) (list 'A 'F 'E))))

   (test-case
    "No path found"
    (let ([g (hash
              'A (hash 'C 1 'B 1)
              'B (hash 'A 1 'C 1 'D 1)
              'C (hash 'A 1 'B 1 'D 1)
              'D (hash 'B 1)
              'E (hash 'F 1)
              'F (hash 'E 1))])
      (check-exn exn:fail? (lambda () (dijkstra g #:start 'A #:end 'E)))))

   (test-case
    "Weighted graph"
    (let ([g (hash
              'A (hash 'C 9 'B 7 'F 16)
              'B (hash 'A 7 'C 10 'D 15)
              'C (hash 'A 9 'B 10 'D 11)
              'D (hash 'B 15 'E 6)
              'E (hash 'F 11 'D 6)
              'F (hash 'A 16 'E 11))])
      (check-equal? (unwind-path (dijkstra g #:start 'A #:end 'E) 'A 'E) (list 'A 'C 'D 'E))))

;   (test-case
;    "Weighted graph"
;    (let* ([g (hash
;              'A (hash 'B 4 'H 8)
;              'B (hash 'A 4 'C 8 'H 11)
;              'C (hash 'B 8 'D 7 'F 4 'I 2)
;              'D (hash 'C 7 'E 9 'F 14)
;              'E (hash 'D 9 'F 10)
;              'F (hash 'C 4 'D 14 'E 10 'G 2)
;              'G (hash 'F 2 'H 1 'I 6)
;              'H (hash 'A 8 'B 11 'G 1 'I 7)
;              'I (hash 'C 2 'H 7 'G 6)
;              )]
;          [expected (hash
;                     'A (mutable-set 'H 'B)
;                     'C (mutable-set 'D 'I)
;                     'D (mutable-set 'E)
;                     'F (mutable-set 'C)
;                     'G (mutable-set 'F)
;                     'H (mutable-set 'G))]
;          [actual (prim-mst g 'A)]
;          [is-equal (equal? actual expected)])
;      (check-equal? is-equal #t)))))
   ))

(run-tests graph-tests)