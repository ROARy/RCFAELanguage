package cs214.language

abstract class Expression {
    
    override def toString() : String = {
        "(Expression)"
    }
}

/*
<RCFAE> ::=   <num>
x			| {+ <RCFAE> <RCFAE>} 			2 arg
x			| {* <RCFAE> <RCFAE>} 			2 arg
			| <id> 							1 arg
x			| {fun {<id>} <RCFAE>}			2 arg
			| {<RCFAE> <RCFAE>}				1 arg 
x			| {if0 <RCFAE> <RCFAE> <RCFAE>}	3 arg
x			| {rec {<id>} {<RCFAE>} <RCFAE>}2 arg
x			| {with {{<id>} <RCFAE>} {<RCFAE>}} 3 arg
 */

/*
#lang plai

(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)] 
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

(define-type CFAE/L-Value 
  [numV (n number?)] 
  [closureV (param symbol?) (body CFAE/L?) (env Env?)]
  [exprV (expr CFAE/L?) (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (env Env?)])

;;; Problem 4 from Lab2
;; New CFAE/L parser that handles functins, applications
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (cond
       [(empty? sexp) '()]
       [(list? (first sexp)) 
        (app (parse (first sexp)) (parse (second sexp)))]
       [else (case (first sexp)
          [(+) (add (parse (second sexp)) 
                    (parse (third sexp)))]
          [(with) (app (fun (first (second sexp)) (parse (third sexp))) (parse (second (second sexp))))]
          [(fun) (fun (first (second sexp))
                      (parse (third sexp)))]
          [else (app (parse (first sexp))
                     (parse (second sexp)))]
          )])]))

;; Wrapper that calls parse, interp and strict
(define (go expr)
  (strict (interp (parse expr) (mtSub))))

;; num+ : CFAE/L-Value CFAE/L-Value -> numV
(define (num+ n1 n2)
  (numV (+ (numV-n (strict n1)) (numV-n (strict n2)))))

;; num-zero? : CFAE/L-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n (strict n))))

;; strict : CFAE/L-Value -> CFAE/L-Value [excluding exprV] 
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (let ((the-value (strict (interp expr env))))
             (printf "Forcing exprV to ~a~n" the-value)
             the-value)]
    [else e]))

;; lookup : symbol->Env->CFAE/L-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))

;; interp : CFAE/L Env -> CFAE/L-Value
(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))] 
    [id (v) (lookup v env)]
    [fun (bound-id bound-body) (closureV bound-id bound-body env)] 
    [app (fun-expr arg-expr)
         (let ((fun-val (strict (interp fun-expr env)))
               (arg-val (exprV arg-expr env)))
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         arg-val
                         (closureV-env fun-val))))]))

*/

