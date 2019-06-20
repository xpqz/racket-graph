#lang racket

(require "graph.rkt")
(require rackunit)
(require rackunit/text-ui)

(define (mhash . xs)
  (let ([ht (make-hash)])
    (for ([pair (in-slice 2 xs)])
      (apply hash-set! ht pair))
    ht))

(define graph-tests
  (test-suite
   "Tests for dijkstra [graph.rkt]"
   
   (test-case
    "Simple grid-like graph"
    (let ([g (make-graph
              (edge 'A 'C 1)
              (edge 'A 'B 1)
              (edge 'A 'F 1)
              (edge 'B 'C 1)
              (edge 'B 'D 1)
              (edge 'C 'D 1)
              (edge 'D 'E 1)
              (edge 'E 'F 1))])
      (check-equal? (unwind-path (dijkstra g #:start 'A #:end 'E) 'A 'E) (list 'A 'F 'E))))

   (test-case
    "No path found"
    (let ([g (make-graph
              (edge 'A 'C 1)
              (edge 'A 'B 1)
              (edge 'B 'C 1)
              (edge 'B 'D 1)
              (edge 'C 'D 1)
              (edge 'E 'F 1))])
      (check-exn exn:fail? (lambda () (dijkstra g #:start 'A #:end 'E)))))

   (test-case
    "Weighted graph"
    (let ([g (make-graph
              (edge 'A 'C 9)
              (edge 'A 'B 7)
              (edge 'A 'F 16)
              (edge 'B 'C 10)
              (edge 'B 'D 15)
              (edge 'C 'D 11)
              (edge 'D 'E 6)
              (edge 'E 'F 11))])
      (check-equal? (unwind-path (dijkstra g #:start 'A #:end 'E) 'A 'E) (list 'A 'C 'D 'E))))

   (test-case
    "Weighted graph MST"
    (let* ([g (make-graph
               (edge 'A 'B 4)
               (edge 'A 'H 8)
               (edge 'B 'C 8)
               (edge 'B 'H 11)
               (edge 'C 'D 7)
               (edge 'C 'F 4)
               (edge 'C 'I 2)
               (edge 'D 'E 9)
               (edge 'D 'F 14)
               (edge 'E 'F 10)
               (edge 'F 'G 2)
               (edge 'G 'H 1)
               (edge 'G 'I 6)
               (edge 'H 'I 7))]
          [expected (mhash
                     'A (mutable-set 'H 'B)
                     'C (mutable-set 'D 'I)
                     'D (mutable-set 'E)
                     'F (mutable-set 'C)
                     'G (mutable-set 'F)
                     'H (mutable-set 'G))])
      (check-equal? (prim-mst g 'A) expected)))

   (test-case
    "Breadth-first-search"
    (let* ([g (make-graph
              (edge 'Frankfurt 'Mannheim 85)
              (edge 'Frankfurt 'Wurzburg 217)
              (edge 'Frankfurt 'Kassel 173)
              (edge 'Mannheim 'Karlsruhe 80)
              (edge 'Wurzburg 'Erfurt 186)
              (edge 'Wurzburg 'Nurnberg 103)
              (edge 'Nurnberg 'Stuttgart 183)
              (edge 'Nurnberg 'Munchen 167)
              (edge 'Kassel 'Munchen 502)
              (edge 'Karlsruhe 'Augsburg 250)
              (edge 'Augsburg 'Munchen 84))]
          [came-from (breadth-first-search g 'Frankfurt)]
          [expected (mhash
                     'Augsburg 'Munchen
                     'Erfurt 'Wurzburg
                     'Frankfurt #f
                     'Karlsruhe 'Mannheim
                     'Kassel 'Frankfurt
                     'Mannheim 'Frankfurt
                     'Munchen 'Kassel
                     'Nurnberg 'Wurzburg
                     'Stuttgart 'Nurnberg
                     'Wurzburg 'Frankfurt)])
      (check-equal? came-from expected)))))

(run-tests graph-tests)