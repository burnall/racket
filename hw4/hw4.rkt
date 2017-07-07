
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
     null
     (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))])) 

(define (stream-for-n-steps s n)
  (letrec ([read-from-stream (lambda (s n)
                               (if (= n 0)
                                 null  
                                 (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))])
    (sort (read-from-stream s n) <)))

(define (funny-number-stream)
  (letrec ([f (lambda (n)
              (cons (if (= 0 (remainder n 5)) (- n) n) (lambda () (f (+ n 1)))))])
    (f 1)))

(define (dan-then-dog)
  (letrec ([f (lambda (s)
              (cons s (lambda () (f (if (equal? s "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (f "dan.jpg")))

(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([iter (lambda (i)
                (cond [(= i (vector-length vec)) #f]
                      [(not (pair? (vector-ref vec i))) (iter (+ i 1))]
                      [(equal? (car (vector-ref vec i)) v) (vector-ref vec i)]
                      [#t (iter (+ i 1))]))])
    (iter 0)))

(define (cached-assoc xs n)
  (letrec([cache (make-vector n #f)]
          [cnt 0]
          [f (lambda (v)
               (let ([found (vector-assoc v cache)])
                 (if found
                     found
                     (let ([found (assoc v xs)])
                       (if found
                           (begin
                             (vector-set! cache cnt found)
                             (set! cnt (if (< cnt (- n 1)) (+ 1 cnt) 0))
                             found)
                           #f)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
            [f (lambda ()
                 (let ([v e2])
                   (if (< v v1) (f) #t)))])
       (f))]))
