#lang racket

(require racket/hash)
(require "priority-queue.rkt")

(provide dijkstra unwind-path prim-mst)

(define-struct node (item cost) #:transparent)

(define (unwind-path came-from start end)
  (let loop ([current end] [path '()])
    (cond [(equal? current start) (cons start path)]
          [else
           (loop (hash-ref came-from current) (cons current path))])))

(define (frontier-nodes graph cost-so-far current)
  (for*/list ([(elem cost) (in-hash (hash-ref graph current))]
              [new-cost (in-value (+ (hash-ref cost-so-far current) cost))]
              #:when (or
                      (not (hash-has-key? cost-so-far elem))
                      (< new-cost (hash-ref cost-so-far elem))))
    (make-node elem new-cost)))

(define dijkstra
  (λ (graph #:start start #:end end)
    (let loop ([frontier (make-queue 0 start)]
               [came-from (make-hash (list (cons start #f)))]
               [cost-so-far (make-hash (list (cons start 0)))])
  
      (cond [(empty? frontier) (error "No path found")]
            [else
             (let ([current (pop-queue frontier)])
               (cond [(equal? current end) came-from]
                     [else
                      (let ([nodes (frontier-nodes graph cost-so-far current)])
                        (for ([emt nodes])
                          (push-queue! frontier (node-cost emt) (node-item emt)))
                        (apply hash-set*! came-from (flatten (for/list ([emt nodes]) (cons (node-item emt) current))))
                        (apply hash-set*! cost-so-far (flatten (for/list ([emt nodes]) (cons (node-item emt) (node-cost emt)))))
                        (loop frontier came-from cost-so-far))]))]))))

(define (prim-mst graph start)
  (let ([edges (make-queue)])
    (for ([(to cost) (in-hash (hash-ref graph start))])
      (push-queue! edges cost (cons start to)))
    
    (let loop ([mst (make-hash)] [edges edges] [visited (mutable-set start)])
      (cond [(empty? edges) mst]
            [else
             (let* ([edge (pop-queue edges)]
                    [from (car edge)]
                    [to (cdr edge)])
                 
               (unless (set-member? visited to)
                 (set-add! visited to)
                 (unless (hash-has-key? mst from)
                   (hash-set! mst from (mutable-set)))
                 (set-add! (hash-ref mst from) to))

               (for ([(next cost) (in-hash (hash-ref graph to))])
                 (unless (set-member? visited next)
                   (push-queue! edges cost (cons to next))))
               
               (loop mst edges visited))]))))
                          
                 