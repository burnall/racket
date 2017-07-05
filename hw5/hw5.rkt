;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))
  ))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str env)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
    (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        [(int? e) e]
        
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]

        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
               [env-new (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) env-new))]

        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))] 
        
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to a non-apair")))]

        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to a non-apair")))]
        
        [(aunit? e) e]

        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]

        [(fun? e) (closure env e)]

        [(closure? e) e]
         
        [(call? e)
         (let ([cloj (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cloj)
               (let* ([f (closure-fun cloj)]
                      [new-env (cons (cons (fun-formal f) arg) (closure-env cloj))]
                      [new-env (if (fun-nameopt f)
                                   (cons (cons (fun-nameopt f) cloj) new-env)
                                   new-env)])
                 (eval-under-env (fun-body f) new-env))
               (error "MUPL call applied to nonfunction")))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
     (let ([e (car lstlst)])
       (mlet (car e) (cdr e) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4
(define mupl-map
  (fun #f "f"
       (fun "iter" "xs"
            (ifaunit (var "xs")
                      (aunit)
                      (apair (call (var "f") (fst (var "xs")))
                             (call (var "iter") (snd (var "xs"))))))))
                   

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n" (call (var "map") (fun #f "elem" (add (var "elem") (var "n"))))))) 

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (struct (res e fv))
  (define (f e)
     (cond
        [(var? e)
         (res e (set (var-string e)))]

        [(int? e)
         (res e (set))]

        [(add? e)
         (let ([r1 (f (add-e1 e))]
               [r2 (f (add-e2 e))])
          (res (add (res-e r1) (res-e r2))
               (set-union (res-fv r1) (res-fv r2))))]
        
        [(ifgreater? e)
         (let ([r1 (f (if-greater-e1 e))]
               [r2 (f (if-greater-e2 e))]
               [r3 (f (if-greater-e3 e))]
               [r4 (f (if-greater-e4 e))])
          (res (ifgreater (res-e r1) (res-e r2) (res-e r3) (res-e r4))
              (set-union (res-fv r1) (res-fv r2) (res-fv r3) (res-fv r4)))
               
        [(mlet? e)
         (let ([r-e (f (mlet-e e))]
               [r-body (f (mlet-body e))])
           (res (mlet (mlet-var e) (res-e r-e) (res-e r-body))
                (set-union (res-fv r-e)
                    (set-remove (res-fv r-body) (mlet-var e)))))]

        [(apair? e)
         (set-union (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
         
        [(fst? e) (compute-free-vars (fst-e e))]
         
        [(snd? e) (compute-free-vars (snd-e e))]

        [(aunit? e) (set)]
         
        [(isaunit? e) (compute-free-vars (isaunit-e e))]

        [(fun? e)
         (set-subtract (compute-free-vars (fun-body e))
                       (set (fun-formal e))
                       (if (fun-nameopt e) (set (fun-nameopt e)) (set)))]
         
        [(closure? e) (compute-free-vars (closure-fun e))]

        [(call? e) (compute-free-vars (call-funexp e))]
         
        [#t (error (format "bad MUPL expression: ~v" e))]))


;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
