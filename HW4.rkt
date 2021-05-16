#lang plai
;;Written by Mick Scanlon oct. 2020
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [mult (lhs WAE?) (rhs WAE?)]
  [div (lhs WAE?) (rhs WAE?)]
  [minus (lhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [if<0 (expr WAE?)(val1 WAE?)(val2 WAE?)]
  [id (name symbol?)]
 )

;; parse : sexp -> WAE
;; to convert s-expressions into WAEs
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(and (list? sexp) (= (length sexp) 2))
         (case (first sexp)
           [(-) (minus (parse (second sexp)))]
           [else (error 'parse "Incorrect Syntax")]
           )]
        [(and (and (list? sexp) (= (length sexp) 3)) (and (list? (second sexp)) (= (length sexp) 2))) 
         (case (first sexp)

           )]
        [(and (list? sexp) (= (length sexp) 3))
         (case (first sexp)
           [(+) (add (parse (second sexp))
                     (parse (third sexp)))]
           [(-) (sub (parse (second sexp))
                     (parse (third sexp)))]         
           [(*) (mult (parse (second sexp))
                      (parse (third sexp)))]
           [(/) (div (parse (second sexp))
                     (parse (third sexp)))]
           [(with) (with (first (second sexp))
                         (parse (second (second sexp)))
                         (parse (third sexp)))]
           [else(error 'parse "Incorrect Syntax")]
           )]
        [(and (list? sexp)(= (length sexp) 4))
         (case (first sexp)
           [(if<0) (if<0 (parse (second sexp))
                         (parse (third sexp))
                         (parse (fourth sexp)))]
           [else(error 'parse "Incorrect Syntax")]
         )]
        [else (error 'parse "Unknown syntax")]
   ))

;subst: WAE symbol WAE --> WAE
;
; Example:
; > (subst (with 'y (add (id 'x) (num 2)) (add (id 'y)(id 'x))) 'x (num 5))
; (with 'y (add (num 5) (num 2)) (add (id 'y) (num 5)))
;

; FINAL (CORRECT) VERSION
(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [div (l r) (div (subst l sub-id val)
                    (subst r sub-id val))]
    [mult (l r) (mult (subst l sub-id val)
                      (subst r sub-id val))]
    [if<0 (l r i) (if<0 (subst l sub-id val)
                        (subst r sub-id val)
                        (subst i sub-id val))]
    [minus (l) (minus (subst l sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val) ; we need to subst into the named-expr
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val) ; we need to subst into the named-expr
                    (subst bound-body sub-id val)))] ; as well as the body.
    [id (v) (if (symbol=? v sub-id) val expr)]
 ))

; calc: WAE --> number
(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [div (l r) (quotient (calc l) (calc r))]
    [minus (l) (*(calc l) -1)]
    [mult (l r) (* (calc l) (calc r))]
    [if<0 (l r i) (if (< (calc l) 0) (calc r) (calc i))] 
    [with (bound-id named-expr bound-body)
          (calc (subst bound-body
                       bound-id
                       (num (calc named-expr))))]
    [id (v) (error 'calc "free identifier ~a" v)]
  ))

(define (run s)
  (calc (parse s)))