#lang racket/base

(require struct-plus-plus
         racket/require
         (multi-in racket (contract hash format function match)))

(provide make-queue
         queue?
         queue-count
         queue-empty?
         queue-add
         queue-add*
         queue-remove
         queue-member?
         queue.id
         )

(struct++ queue ([(id (gensym "queue-")) symbol?]
                 [(head-id 0)            natural-number/c]
                 [(next-id 0)            natural-number/c]
                 [(items (hash))         hash?])
          (#:rule ("head-id points to valid item or items are empty"
                   #:check (head-id items) [(or (hash-empty? items)
                                                (hash-has-key? items head-id))])))

(define (make-queue) (queue++))

(define/contract (queue-count q)
  (-> queue? exact-nonnegative-integer?)
  (hash-count (queue.items q)))

;;----------------------------------------------------------------------

(define/contract (queue-empty? q)
  (-> queue? boolean?)
  (hash-empty? (queue.items q)))

;;----------------------------------------------------------------------

(define/contract (queue-add* q . items)
  (->* (queue?) () #:rest (listof any/c) queue?)
  (match-define (struct* queue ([head-id head-id]
                                [next-id next-id]
                                [items current-items]))
    q)
  (define new-next-id (+ next-id (length items)))
  (queue++ #:head-id head-id
           #:next-id new-next-id
           #:items (hash-union current-items
                               (for/hash ([item items]
                                          [idx  (in-range next-id new-next-id)])
                                 (values idx item)))))

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

  (match-define (struct* queue ([head-id head-id]
                                [next-id next-id]
                                [items items]))
    q)
  (values
   (hash-ref (queue.items q) head-id)
   (queue++ #:head-id (add1 head-id)
            #:next-id next-id
            #:items   (hash-remove (queue.items q) head-id))))

;;----------------------------------------------------------------------

(define/contract (queue-member? q val #:key [key identity] #:equal [comparator equal?])
  (->* (queue? any/c)
       (#:key (-> any/c any/c) #:equal (-> any/c any/c any/c))
       boolean?)
  (for/or ([item (in-list (hash-values (queue.items q)))])
    (comparator val (key item))))

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
       (loop (sub1 num) (add1 correct) new-q)))
   )

  (test-suite
   "queue-member?"

   (define q (queue-add* (make-queue) 1 2 3 4 5))

   (ok (queue-member? q 5) "(queue-member? q 5) is true")
   (is-false (queue-member? q 16) "(queue-member? q 16) is false, as expected")

   (define q2 (queue-add* (make-queue) (hash 'a 1 'b 2)  (hash 'a 7 'c 3 'd 4)))
   (ok (queue-member? q2 7 #:key (curryr hash-ref 'a #f))
       "(queue-member? q2 7 #:key (curryr hash-ref 'a #f)) is true")
   (is-false (queue-member? q2 7 #:key (curryr hash-ref 'apple #f))
             "(queue-member? q2 7 #:key (curryr hash-ref 'apple #f)) is false, as expected")

   (struct thingy (id color))
   (define q3 (queue-add* (make-queue) (thingy '(j) 'purple) (thingy '(a b c) 'red)))
   (ok (queue-member? q3 3 #:key (compose1 length thingy-id) #:equal eq?)
       "(queue-member? q3 3 #:key (compose1 length thingy-id) #:equal eq?)")

   (define q4 (queue-add* (make-queue)
                          (cons 'foo (thunk 7))
                          (cons 'bar (thunk 7))
                          (cons 'baz (thunk 7))
                          (cons 'jaz (thunk 7))))
   (ok (queue-member? q4 'foo #:key car)
       "(queue-member? q4 'foo #:key car)")
   )

  (done-testing)
  )
