#lang racket
; Written by Mick Scanlon Sept. 2020
; in-range?: number number (atom list) --> boolean
(define in-range?
   (lambda (x y s)
    (cond [(null? s) #t] ; Checks for empty list, returns true since recursion would be complete
          [(> x (car s)) #f] ; If lower bound is greater than the element return False
          [(< y (car s)) #f] ; If upper bound is less than the element return False
          [else (in-range? x y (cdr s)) ] ; Recursive call
)))
; atom-count: atom (atom list) --> number
(define atom-count
  (lambda (x s)
    (cond [(null? s) 0] ; Is list empty? 
          [(eq? (car s) x) (+ 1 (atom-count x (cdr s)))] ; Are the two atoms equal? if so adds to count and calls recursively 
          [else (atom-count x (cdr s))] ; If not equal calls recursively 
)))
; lookup: atom (atom list) --> atom
(define lookup
  (lambda (x s)
    (cond [(null? s) 'UNKOWN] ; Empty list returns 
          [(eq? x (car (car s))) (car(cdr(car s)))] ; Is specified atom equal to first atom of first pair(also an atom) if so, returns second atom in pair 
          [else (lookup x (cdr s))] ; Otherwise recursive call 
)))