#lang plai-typed
;; Data definition msl (my simple language)
;; msl is a number or an addition of two msl
(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-div (lhs : msl) (rhs : msl)]
  [msl-pow (lhs : msl) (rhs : msl)]
  [msl-min (l : msl)]
  [msl-great (l : msl) (m : msl) (r : msl)]
  
  )
;; eval msl -> number
;; take a simple abstract arithmetic expression involving only binary additions
;; and evaluate it
;; Examples
;; (msl-num 42) -> 42
;; (msl-num 99) -> 99
;; (msl-add (msl-num 42) (msl-num 99)) -> 141

(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (- (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
    [msl-div (lhs rhs) (/ (eval lhs) (eval rhs))]
    [msl-pow (lhs rhs) (pow (eval lhs) (eval rhs))]
    [msl-min (l) (- 0 (eval l))]
    [msl-great (l m r) (if-greater-than-zero (eval l) (eval m) (eval r))]
   ))

(define (pow u t )
  (cond
    ((= u 1) t)
    (else
     (* t (pow (sub1 u)t)))))

;;(eval (msl-add (msl-num 42) (msl-num 23)))
;;(eval (msl-sub (msl-num 42) (msl-num 23)))
;;(eval (msl-mul (msl-num 7) (msl-num 8)))
;;(eval (msl-div (msl-num 99) (msl-num 11)))
;;(eval (msl-pow (msl-num 2) (msl-num 3)))


;;test
(test (eval (msl-add (msl-num 42) (msl-num 23))) 65)
(test (eval (msl-sub (msl-num 42) (msl-num 23))) 19)
(test (eval (msl-mul (msl-num 7) (msl-num 8))) 40)
(test (eval (msl-div (msl-num 99) (msl-num 11))) 9)
(test (eval (msl-pow (msl-num 2) (msl-num 3))) 9)


;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; Grammar: S1={+,-.*,**}, S2=[0-9], S=S1 S S/S2
;; Examples:
;; '3 -> (msl-num 3)
;; '(+ 2 4) -> (msl-add (msl-num 2) (msl-num 4))
;; '(- 3) -> (msl-min (msl-num 3))
;; '(- 3 1) -> (msl-sub (msl-num 3) (msl-num 1))
;; '(* 3 2) -> (msl-mul (msl-num 3) (msl-num 2))
;; '(** 2 5) -> (msl-pow (msl-num 2) (msl-num 5))

(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(-) (cond [(= 2 (length sl)) (msl-min (parse (second sl)))] ;; if symbol is (-) and there is no third exp, (msl-min second)
              [else (msl-sub (parse (second sl)) (parse (third sl)))]) ;; else (msl-sub second third)
              ]
         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
         [(**) (msl-pow (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]
    ))


;;Tests
(test (parse '(- 3)) (msl-min (msl-num 3)))
(test (parse '(- 8)) (msl-min (msl-num 8)))
(test (parse '(- -3)) (msl-min (msl-num -3)))
(test (parse '(+ (- 3) (* 5 (- (* 7 6))))) (msl-add (msl-min (msl-num 3)) (msl-mul (msl-num 5) (msl-min (msl-mul (msl-num 7) (msl-num 6)))))) 
(test (eval (parse '(+ (- 3) (* 5 (- (* 7 6)))))) -213)

;;output examples
(eval (parse '(+ 5 3))) ;8
(eval (parse '(+ (- 3 2) 4))) ;5
(eval (parse '(* (+ 1 2) (- 3)))) ;-9
(eval (parse '(** 2 4))) ;16


;"Example outputs"
;(output-reverse-polish (msl-num 7))
;(output-reverse-polish (msl-add (msl-num 3) (msl-num 4)))
;(output-reverse-polish (msl-mul (msl-num 3) (msl-num 4)))
;(output-reverse-polish (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)))
;(output-reverse-polish (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9))))


;; Parser -> reverse polish output 
;; Example:
;; (output-reverse-polish (parse '(+ 99 (* 5 8))))
;; Parser -> evaluation 
;; Example:
;; (eval (parse '(+ 99 (* 5 8))))

;;Sugaring
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [subS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [powS (l : ArithS) (r : ArithS)]
  [unaryS (l : ArithS)])

;;Desugaring
;; parse ArithS -> msl
;; convert a ArithS into the equivalent msl form
;; Grammar:
;; Examples:
;; (desugar (plusS (numS 7) (numS 7))) (msl-add (msl-num 7) (msl-num 7)))
;; (desugar (plusS (numS 5) (unaryS (numS 3)))) (msl-add (msl-num 5) (msl-min (msl-num 3))))
;; (desugar (subS (numS 9) (numS 0))) (msl-sub (msl-num 9) (msl-num 0)))
;; (desugar (subS (numS 1) (numS 3))) (msl-sub (msl-num 1) (msl-num 3)))
;; (desugar (multS (numS 7) (numS 2))) (msl-mul (msl-num 7) (msl-num 2)))

(define (desugar [as : ArithS]) : msl
  (type-case ArithS as
    [numS (n) (msl-num n)]
    [plusS (l r) (msl-add (desugar l)
                           (desugar r))]
    [multS (l r) (msl-mul (desugar l)
                           (desugar r))]
    [subS (l r) (msl-sub (desugar l)
                          (desugar r))]
    [powS (l r) (msl-pow (desugar l)
                          (desugar r))]
    [unaryS (l) (msl-min (desugar l))]))

(test (desugar (plusS (numS 7) (numS 7))) (msl-add (msl-num 7) (msl-num 7)))
(test (desugar (plusS (numS 5) (unaryS (numS 3)))) (msl-add (msl-num 5) (msl-min (msl-num 3))))
(test (desugar (subS (numS 9) (numS 0))) (msl-sub (msl-num 9) (msl-num 0)))
(test (desugar (subS (numS 1) (numS 3))) (msl-sub (msl-num 1) (msl-num 3)))
(test (desugar (multS (numS 7) (numS 2))) (msl-mul (msl-num 7) (msl-num 2)))
(test (desugar (multS (numS 18) (numS 0))) (msl-mul (msl-num 18) (msl-num 0)))
(test (desugar (powS (numS 5) (numS 2))) (msl-pow (msl-num 5) (msl-num 2)))
(test (desugar (powS (numS 2) (numS 8))) (msl-pow (msl-num 2) (msl-num 8)))
(test (desugar (unaryS (numS 2))) (msl-min (msl-num 2)))
(test (desugar (unaryS (numS 18))) (msl-min (msl-num 18)))


(define (parser-infix [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond 
         [(s-exp-symbol? (first sl))
          (case (s-exp->symbol (first sl))
            [(-) (unaryS (parser-infix (second sl)))]
            [else (error 'parser-infix "invalid input")]
            )]
         [(s-exp-number? (first sl))
          (case (s-exp->symbol (second sl))
            [(+) (plusS (parser-infix (first sl)) (parser-infix (third sl)))]
            [(-) (subS (parser-infix (first sl)) (parser-infix (third sl)))]
            [(*) (multS (parser-infix (first sl)) (parser-infix (third sl)))]
            [(**) (powS (parser-infix (first sl)) (parser-infix (third sl)))]
            [else (error 'parser-infix "invalid list input")])]
         [else (error 'parser-infix "invalid input")]))]
     [else (error 'parser-infix "invalid input")]))

(test (parser-infix '(- 3)) (unaryS (numS 3)))
(test (parser-infix '(2 + 3)) (plusS (numS 2) (numS 3)))
(test (parser-infix '(6 - 4)) (subS (numS 6) (numS 4)))
(test (parser-infix '(2 ** 2)) (powS (numS 2) (numS 2)))
(test (parser-infix '(10 * 20)) (multS (numS 10) (numS 20)))

;; list of expression -> expression
;; convert list of expression to expression
;; Grammar:
;; Examples:
;; (if-greater-than-zero 3 2 1) 2)
;; (if-greater-than-zero -3 5 9) 9)
;; (if-greater-than-zero -5 6 5) 5)
;; (if-greater-than-zero  5 6 5) 6) 
;; (if-greater-than-zero 10 20 -5) 20)
;; (if-greater-than-zero -1 -3 5) 5)

(define (if-greater-than-zero expression1 expression2 expression3)
  (cond
    [(> expression1 0) expression2]
    [else expression3]))

(test (if-greater-than-zero 3 2 1) 2)
(test (if-greater-than-zero -3 5 9) 9)
(test (if-greater-than-zero -5 6 5) 5)
(test (if-greater-than-zero  5 6 5) 6)
(test (if-greater-than-zero  10 20 -5) 20)
(test (if-greater-than-zero  -1 -3 5) 5)







