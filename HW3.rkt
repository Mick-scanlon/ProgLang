#lang racket
;; Written by Mick Scanlon sept. 2020
;; updown: list-of-numbers --> list-of-numbers
(define updown
  (lambda (s)
    (cond [(null? s) '()] ;; empty list returns empty list
          [(even? (car s)) (cons (+ (car s) 1) (updown (cdr s)))] ;; if even number adds 1 to element and adds to new list
          [(odd? (car s)) (cons (- (car s) 1) (updown (cdr s)))] ;; if odd number subtracts 1 from element and adds to new list
)))
;; zip: list list --> list-of-pairs
(define zip
  (lambda (a s)
    (cond [(eq? (length a)(length s)) #t] ;; checks that lists are equal length 
          [else (error 'zip "lists not equal length")]) ;; if not returns error message
    (cond [(null? a) '()] ;; empty list returns empty list
          [(cons (list (car a) (car s)) (zip (cdr a) (cdr s)))] ;; takes car a and car s and makes a pair, cons with recursive call 
)))
;; deep-mult: list-of-numbers --> number
(define deep-mult
  (lambda (s)
    (cond [(null? s) 1] ;; multiplicative identity
          [(number? (car s)) (* (car s) (deep-mult (cdr s)))] ;; car of s is a number so its multiplied by the recursive call of the cdr of s 
          [(list? (car s)) (* (deep-mult (car s)) (deep-mult (cdr s)))] ;; recursive call on the car of s and the cdr
          [else (deep-mult (cdr s))] ;; atom thats not number or a list, recursive call with the cdr s ignoring the car s
)))
;; list --> list-of-atoms
(define drop-parens
  (lambda (s)
    (cond [(null? s) '()] ;; empty list returns empty list
          [(and (not (list? (car s))) (not (pair? (car s)))) (cons (car s) (drop-parens (cdr s)))] ;; if car s is an atom, creates new list with car s as first element and recursive call as second
          [else (append (drop-parens (car s)) (drop-parens (cdr s)))] ;; if list then adds result of recursion to already created list of elements
)))