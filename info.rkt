#lang info
(define collection "queue")
(define deps '("base" "struct-plus-plus"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "test-more"))
(define scribblings '(("scribblings/queue.scrbl" ())))
(define pkg-desc "A FIFO queue struct with amortized O(n) access")
(define version "0.1")
(define pkg-authors '("David K. Storrs"))
(define license '(Apache-2.0 OR MIT))
