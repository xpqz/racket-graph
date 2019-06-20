#lang racket
;;
;; Naive graph stuff for Racket
;;
;; Stefan Kruger (c) 2019
;;

(require data/queue)
(require "heap-queue.rkt")

(provide
 make-graph
 edge
 make-edge
 dijkstra
 unwind-path
 prim-mst)

(struct node (item cost) #:transparent)
(struct edge (start end weight) #:transparent)

(define make-edge
  (λ (start end #:weight [weight 0])
    (edge start end weight)))

(struct graph ([nodes #:mutable] [edges #:mutable]) #:transparent)

(define (add-edge! g edge)
  (let ([start (edge-start edge)]
        [end (edge-end edge)]
        [weight (edge-weight edge)]
        [edges (graph-edges g)])
    (for ([n (list start end)])
      (unless (set-member? (graph-nodes g) n) (set-add! (graph-nodes g) n)))
    
    (cond [(not (hash-has-key? edges start))
           (hash-set! edges start (make-hash (list (cons end weight))))]
          [else
           (hash-set! (hash-ref edges start) end weight)])

    (cond [(not (hash-has-key? edges end))
           (hash-set! edges end (make-hash (list (cons start weight))))]
          [else
           (hash-set! (hash-ref edges end) start weight)])))

(define (make-graph . xs)
  (let ([g (graph (mutable-set) (make-hash))])
    (for ([edge xs])
      (add-edge! g edge))
    g))

(define (unwind-path came-from start end)
  (let loop ([current end] [path '()])
    (cond [(equal? current start) (cons start path)]
          [else
           (loop (hash-ref came-from current) (cons current path))])))

(define (frontier-nodes graph cost-so-far current)
  (for*/list ([(elem cost) (in-hash (hash-ref (graph-edges graph) current))]
              [new-cost (in-value (+ (hash-ref cost-so-far current) cost))]
              #:when (or
                      (not (hash-has-key? cost-so-far elem))
                      (< new-cost (hash-ref cost-so-far elem))))
    (node elem new-cost)))

(define dijkstra
  ;; Dijkstra's shortest path algorithm, with a priority queue.
  ;;
  ;; See https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
  (λ (graph #:start start #:end end)
    (let loop ([frontier (make-heap-queue 0 start)]
               [came-from (make-hash (list (cons start #f)))]
               [cost-so-far (make-hash (list (cons start 0)))])
  
      (cond [(heap-queue-empty? frontier) (error "No path found")]
            [else
             (let ([current (pop-heap-queue frontier)])
               (cond [(equal? current end) came-from]
                     [else
                      (let ([nodes (frontier-nodes graph cost-so-far current)])
                        (for ([emt nodes])
                          (push-heap-queue! frontier (node-cost emt) (node-item emt)))
                        (apply hash-set*! came-from (flatten (for/list ([emt nodes]) (cons (node-item emt) current))))
                        (apply hash-set*! cost-so-far (flatten (for/list ([emt nodes]) (cons (node-item emt) (node-cost emt)))))
                        (loop frontier came-from cost-so-far))]))]))))

(define (prim-mst graph start)
  ;; Prim's algorithm for finding the minimum spanning tree of a graph.
  ;;
  ;; See https://en.wikipedia.org/wiki/Prim%27s_algorithm
  (let ([edges (make-heap-queue)])
    (for ([(to cost) (in-hash (hash-ref (graph-edges graph) start))])
      (push-heap-queue! edges cost (cons start to)))
    
    (let loop ([mst (make-hash)] [edges edges] [visited (mutable-set start)])
      (cond [(heap-queue-empty? edges) mst]
            [else
             (let* ([edge (pop-heap-queue edges)]
                    [from (car edge)]
                    [to (cdr edge)])
                 
               (unless (set-member? visited to)
                 (set-add! visited to)
                 (unless (hash-has-key? mst from)
                   (hash-set! mst from (mutable-set)))
                 (set-add! (hash-ref mst from) to))

               (for ([(next cost) (in-hash (hash-ref (graph-edges graph) to))])
                 (unless (set-member? visited next)
                   (push-heap-queue! edges cost (cons to next))))
               
               (loop mst edges visited))]))))
                          

(define breadth-first-search
  ;; Breadth-first search with potential early exit. Edge cost not accounted for.
  ;; If no end node given, the returned hash holds the shortest paths from
  ;; the start to every other point in the graph.
  ;;
  ;; (define came-from (breadth-first-search graph start #:end end)
  ;; (unwind-path came-from start end)
  
  (λ (graph start #:end [end '()])
    (let ([queue (make-queue)])
      (enqueue! queue start)
      (let loop ([frontier queue] [came-from (make-hash (list (cons start #f)))])
        (cond [(queue-empty? queue) came-from]
              [else
               (let ([current (dequeue! frontier)])
                 (cond [(and (not (null? end)) (equal? current end)) came-from]
                       [else
                        (for ([neighbour (hash-keys (hash-ref graph current))])
                          (enqueue! frontier neighbour)
                          (hash-set! came-from neighbour current))
                        (loop frontier came-from)]))])))))
