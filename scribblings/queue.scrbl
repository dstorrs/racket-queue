#lang scribble/manual
@(require (for-label racket)
          (for-label queue)
          racket/sandbox
          scribble/example)

@title{queue}
@author{David K. Storrs}

@defmodule[queue]

A queue structure with amortized O(n) add and remove.

@section{Synopsis}

@(define eval
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 50])
        (make-evaluator 'racket)))))

@examples[
 #:eval eval
 #:label #f
 (require queue)

(let* ([q (queue-add* (make-queue) 'a 'b 'c)]  (code:comment "make empty queue, add many items")
       [q (queue-add q 'a)])  (code:comment "add only one item.  is alias for (queue-add* q 'a)")
  (displayln (~a "q is queue?: " (queue? q)))
  (displayln (~a "number of items in queue: " (queue-count q)))
  (displayln (~a "queue is empty?: " (queue-empty? q)))
  (define-values (item new-q) (queue-remove q))
  (displayln (~a "originally added 'a as first item, upon remove got: " item))
  (displayln (~a "'b is member of queue?: " (queue-member? q 'b)))
  (displayln (~a "queue ID (useful for debugging): " (queue.id q)))
)]

