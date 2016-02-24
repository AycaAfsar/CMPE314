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
;;(test (eval (msl-add (msl-num 42) (msl-num 23))) 65)
;;(test (eval (msl-sub (msl-num 42) (msl-num 23))) 19)
;;(test (eval (msl-mul (msl-num 7) (msl-num 8))) 40)
;;(test (eval (msl-div (msl-num 99) (msl-num 11))) 9)
;;(test (eval (msl-pow (msl-num 2) (msl-num 3))) 9)


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






