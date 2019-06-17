#lang racket

(require data/heap)
(require rackunit)

(provide queue-item empty? make-queue pop-queue push-queue!)

(define-struct queue-item (priority value))

(define (queue-item<=? x y)
  (<= (queue-item-priority x) (queue-item-priority y)))

(define (empty? priority-queue)
  (= (heap-count priority-queue) 0))

(define (make-queue . xs)
  (let ([priority-queue (make-heap queue-item<=?)])
    (for ([pair (in-slice 2 xs)])
      (heap-add! priority-queue (apply make-queue-item pair)))
    priority-queue))

(define (pop-queue priority-queue)
  (let ([item (heap-min priority-queue)])
    (heap-remove-min! priority-queue)
    (queue-item-value item)))

(define (push-queue! priority-queue priority value)
  (heap-add! priority-queue (make-queue-item priority value)))

