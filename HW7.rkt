#lang plai
;;Written by Mick Scanlon nov. 2020
(define-type TJOE
  [num (n number?)]
  [boo (b boolean?)]
  [id (name symbol?)]
  [add (lhs TJOE?) (rhs TJOE?)]
  [sub (lhs TJOE?) (rhs TJOE?)]
  [mul (lhs TJOE?) (rhs TJOE?)]
  [div (lhs TJOE?) (rhs TJOE?)]
  [minus (n TJOE?)]
  [equl (lhs TJOE?) (rhs TJOE?)]
  [lessthan (lhs TJOE?) (rhs TJOE?)]
  [with (name symbol?) (type Type?) (named-expr TJOE?) (body TJOE?)]
  [iff (expr TJOE?)  (true TJOE?) (false TJOE?)]
  [fun (arg-name symbol?) (type Type?) (body TJOE?)]
  [app (fun-expr TJOE?) (arg TJOE?)]
)

(define-type TJOE-value
  [numV (n number?)]
  [booleanV (b boolean?)]
  [closureV (param symbol?)
            (body TJOE?)
            (env TypeEnv?)]
  )

(define-type Type
  [numType]
  [boolType]
  [funType (domain Type?)
           (codomain Type?)]
  )

(define-type TypeEnv
  [mtTypeSub]
  [aTypeSub (name symbol?) (type Type?) (env TypeEnv?)]
  )

(define (lookup name tenv)
  (type-case TypeEnv tenv
    [mtTypeSub () (error 'lookup "no binding found for id ~a" name)]
    [aTypeSub (bound-name bound-type rest-tenv)
          (if (symbol=? name bound-name)
              bound-type
              (lookup name rest-tenv))]
 ))

(define (type-parse sexp)
  (cond [(eq? sexp 'number) (numType)]
        [(eq? sexp 'boolean) (boolType)]
        [(equal? sexp 'fun) (funType)]
        [else (error 'type-parse "Unrecognized Type")]
   ))

(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(boolean? sexp) (boo sexp)]
        [(eq? (first sexp) '+) (if (= (length sexp) 3)
                                   (add (parse (second sexp)) (parse (third sexp)))
                                   (error 'add "Incorrect syntax! Expect 2 arguments")
                                   )]
        [(eq? (first sexp) '-) (if (or (= (length sexp) 3) (= (length sexp) 2))
                                   (if (null? (cddr sexp)) (minus (parse (second sexp))) (sub (parse (second sexp)) (parse (third sexp))))
                                   (error 'sub "Incorrect Syntax! Expected 2 Arguments")
                                   )]
        [(eq? (first sexp) '*) (if (= (length sexp) 3)
                                   (mul (parse (second sexp)) (parse (third sexp)))
                                   (error 'mul "Incorrect syntax! Expected 2 arguments"))]
        [(eq? (first sexp) '/) (if (= (length sexp) 3)
                                   (div (parse (second sexp)) (parse (third sexp)))
                                   (error 'div "Incorrect syntax! Expected 2 arguments")
                                   )]
        [(eq? (first sexp) '=) (if (= (length sexp) 3)
                                   (equl (parse (second sexp)) (parse (third sexp)))
                                   (error '= "Incorrect syntax! Expected 2 arguments")
                                   )]
        [(eq? (first sexp) '<) (if (= (length sexp) 3)
                                   (lessthan (parse (second sexp)) (parse (third sexp)))
                                   (error '< "Incorrect syntax! Expected 2 arguments")
                                   )]
        [(eq? (first sexp) 'with) (with (first (second sexp)) 
                                            (type-parse (third (second sexp))) 
                                            (parse (fourth (second sexp)))
                                            (parse (third sexp)))]
        ;[(eq? (first sexp) 'with) ;(if (and (= (length sexp) 4) (= (length (second sexp)) 3))
         ;                             (app (fun ((first (second sexp))
          ;                                  (type-parse (third (second sexp)))
           ;                                 (parse (fourth (second sexp)))
            ;                                (parse (third sexp)))))
                                      ;(error 'with "Incorrect Syntax! Expected 2 Arguments, First Argument should be list of two.")
              ;                        ];)]
        [(eq? (first sexp) 'if) (if (= (length sexp) 4)
                                    (iff (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))
                                    (error 'if "Incorrect syntax! Expected 3 arguments"))]
        [(eq? (first sexp) 'fun) (if (= (length sexp) 3)
                                     (fun (first (second sexp))
                                      (type-parse (third (second sexp)))
                                      (parse (fourth (second sexp)))
                                      (parse (third sexp)))
                                     (error 'fun "Incorrect syntax! Expected 2 arguments"))]
        [else (app (parse (first sexp)) (parse (second sexp)))]
))

;(define (interp expr ds)
;  (type-case TJOE expr
;    [num (n) (numV n)]
;    [boo (b) (booleanV b)]
;    [add (l r) (calc '+ (interp l ds) (interp r ds))]
;    [sub (l r) (calc '- (interp l ds) (interp r ds))]
;    [mul (l r) (calc '* (interp l ds) (interp r ds))]
;    [div (l r) (calc '/ (interp l ds) (interp r ds))]
;    [minus (n) (calc '* (numV -1) (interp n ds))]
;    [equl (l r) (equal? (interp l ds) (interp r ds))]
;    [lessthan (l r) (< (numV-n (interp l ds)) (numV-n (interp r ds)))]
;    [iff (e t f) (if (interp e ds) (interp t ds) (interp f ds))]
;    [with (bound-id named-expr bound-body) (interp bound-body (aSub bound-id (interp named-expr ds) ds))]
;    [id (v) (lookup v ds)]
;    [fun (arg body) (closureV arg body ds)]
;;    [app (fun-expr arg-expr);
;         (let ((fun-closure (interp fun-expr ds)))
;           (interp (closureV-body fun-closure)
;                   (aSub (closureV-param fun-closure)
;                         (interp arg-expr ds)
;                         (closureV-ds fun-closure))))]
;))

(define (calc s a b)
  (numV
  (case s
    ['+ (+ (numV-n a) (numV-n b))]
    ['- (- (numV-n a) (numV-n b))]
    ['* (* (numV-n a) (numV-n b))]
    ['/ (quotient (numV-n a) (numV-n b))]
   )))

(define (get-type expr tenv)
  (type-case TJOE expr
    [id (v) (lookup v tenv)]
    [num (n) (numType)]
    [boo (b) (boolType)]
    [add (l r) (if (and (numType? (get-type l tenv))
                        (numType? (get-type r tenv)))
                   (numType)
                   (error 'get-type "wrong type operands for +"))]
    [sub (l r) (if (and (numType? (get-type l tenv))
                        (numType? (get-type r tenv)))
                   (numType)
                   (error 'get-type "wrong type operands for -"))]
    [minus (n) (if (numType? (get-type n tenv))
                   (numType)
                   (error 'get-type "wrong type operand for -"))]
    [mul (l r) (if (and (numType? (get-type l tenv))
                        (numType? (get-type r tenv)))
                   (numType)
                   (error 'get-type "wrong type operands for *"))]
    [div (l r) (if (and (numType? (get-type l tenv))
                        (numType? (get-type r tenv)))
                   (numType)
                   (error 'get-type "wrong type operands for /"))]
    [equl (l r) (if (and (numType? (get-type l tenv))
                          (numType? (get-type r tenv)))
                     (boolType)
                     (error 'get-type "wrong type operands for ="))]
    [lessthan (l r) (if (and (numType? (get-type l tenv))
                         (numType? (get-type r tenv)))
                    (boolType)
                    (error 'get-type "wrong type operands for <"))]
    [with (bound-id id-type named-expr bound-body)
         (if (equal? id-type (get-type named-expr tenv))
             (get-type bound-body (aTypeSub bound-id id-type tenv))
             (error 'get-type "Type mismatch in with declaration"))]
    [iff (tst tv fv) (if (and (and (boolType? (get-type tst tenv))
                                   (numType? (get-type tv tenv)))
                              (numType? (get-type fv tenv)))
                         (numType)
                         (error 'get-type "wrong type operands for iff"))]
    [fun (arg type body) (if (equal? type (get-type arg tenv))
                             (get-type body (aTypeSub arg type tenv))
                             (error 'get-type "Type mismatch in fun declaration"))]
    [app (fun-expr arg-expr) (if (and (funType? (get-type fun-expr tenv))
                                      (numType? (get-type arg-expr tenv)))
                                 (funType)
                                 (error 'get-type "wrong type operands for app"))]
    ))

(define (length list)
  (cond [(null? list) 0] [else (+ 1 (length (cdr list)))]))

(define (run expr)
  (get-type (parse expr) (mtTypeSub)))