#lang racket

(require data/heap)

(provide heap-queue-item heap-queue-empty? make-heap-queue pop-heap-queue push-heap-queue!)

(struct heap-queue-item (priority value))

(define (heap-queue-item<=? x y)
  (<= (heap-queue-item-priority x) (heap-queue-item-priority y)))

(define (heap-queue-empty? heap-queue)
  (= (heap-count heap-queue) 0))

(define (make-heap-queue . xs)
  (let ([heap-queue (make-heap heap-queue-item<=?)])
    (for ([pair (in-slice 2 xs)])
      (heap-add! heap-queue (apply heap-queue-item pair)))
    heap-queue))

(define (pop-heap-queue heap-queue)
  (let ([item (heap-min heap-queue)])
    (heap-remove-min! heap-queue)
    (heap-queue-item-value item)))

(define (push-heap-queue! heap-queue priority value)
  (heap-add! heap-queue (heap-queue-item priority value)))

