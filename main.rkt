#lang racket/base

(require struct-plus-plus racket/contract racket/match)

(provide make-queue
         queue?
         queue-count
         queue-empty?
         queue-add
         queue-add*
         queue-remove)

(struct++ queue ([(head-id 0)    natural-number/c]
                 [(next-id 0)    natural-number/c]
                 [(items (hash)) hash?]))

(define (make-queue) (queue++))

(define/contract (queue-count q)
  (-> queue? exact-nonnegative-integer?)
  (hash-count (queue.items q)))

;;----------------------------------------------------------------------

(define/contract (queue-empty? q)
  (-> queue? boolean?)
  (zero? (queue-count q)))

;;----------------------------------------------------------------------

(define/contract (queue-add* q . items)
  (->* (queue?) () #:rest (listof any/c) queue?)
  (for/fold ([q q])
            ([item items])
    (match-define (struct queue (head-id next-id items)) q)
    (queue++ #:head-id head-id
             #:next-id (add1 next-id)
             #:items   (hash-set items next-id item))))

(define (queue-add q item)
  (queue-add* q item))

;;----------------------------------------------------------------------

(define/contract (queue-remove q)
  (->i ([q queue?])
       #:pre (q) (or (hash-has-key? (queue.items q) (queue.head-id q))
                     (raise-arguments-error 'queue-remove "tried to remove from an empty queue"))
       (values [item  any/c]
               [new-q queue?])
       #:post (q) (<= (queue.head-id q) (queue.next-id q))
       )

  (match-define (struct queue (head-id next-id items)) q)
  (values
   (hash-ref (queue.items q) head-id)
   (queue++ #:head-id (add1 head-id)
            #:next-id next-id
            #:items   (hash-remove (queue.items q) head-id))))


;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------

(module+ test
  (require test-more)
  (test-suite
   "creating and counting"

   (let ([q (make-queue)])
     (is-type q queue? "created a queue")
     (is (queue-count q) 0 "queues start empty")
     (is (queue-count (queue-add q 7)) 1 "queue-add adds one item")
     (is (queue-count (queue-add* q 7 8 9)) 3 "queue-add* adds multiple items")))

  (test-suite
   "queue-add, queue-add*, and queue-remove respect FIFO"

   (let loop ([num     5]
              [correct 1]
              [q       (queue-add* (make-queue) 1 2 3 4 5)])
     
     (is (queue-count q)
         num
         (format "queue-remove: as expected, on loop iteration #~a we had ~a entries"
                 (- 5 num)
                 num))

     (when (not (zero? num))
       (define-values  (elem new-q) (queue-remove q))
       (is elem correct "got the expected element")
       (loop (sub1 num) (add1 correct) new-q))))

  )
