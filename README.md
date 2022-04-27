queue
=====

A Racket package for functional FIFO queues.

NB:  This module is for regular Racket.  For *Typed* Racket, https://docs.racket-lang.org/functional-data-structures/Queues.html may or may not be better for your needs.


(define q (make-queue))

(queue-empty? q) ; #t
(queue-count q)  ; 0

(define q1 (queue-add q 'apple))
(queue-count q1)  ; 1
(queue-remove q1) ; (values 'apple (make-queue))
(queue-count q1) ; 1

(define q2 (queue-add q1 'banana))
(queue-count q2)  ; 2
(queue-remove q2) ; (values 'apple (queue-add (make-queue) 'banana))
(queue-count q2) ; 2  remember that this is functional, so q2 didn't change

(define q5 (queue-add* (make-queue) 'apple 'banana 'carrot 'dandelion 'eggplant))
; 'apple is the first item in the queue, 'eggplant is last
(queue-count q5)  ; 5
(define-values (elem new-q) (queue-remove q5))
; elem is 'apple
; new-q is (queue 'banana 'carrot 'dandelion 'eggplant)
(queue-count q5)    ; 5 
(queue-count new-q) ; 4
