package cs214.language

/*
<RCFAE> ::=   <num>
			| {+ <RCFAE> <RCFAE>}
			| {* <RCFAE> <RCFAE>}
			| <id>
			| {fun {<id>} <RCFAE>}
			| {<RCFAE> <RCFAE>} 
			| {if0 <RCFAE> <RCFAE> <RCFAE>}
			| {rec {<id>} {<RCFAE>} <RCFAE>}
*/

abstract class Expression
case class NumericExpression(num: Int) extends Expression
case class ConditionalExpression(test: Expression, truth: Expression, falsity: Expression) extends Expression
case class FunctionExpression(id: String, body: Expression) extends Expression
case class RecursiveFunctionExpression(id: String, expr: Expression, body: Expression) extends Expression
case class ApplicationExpression(funExpr: Expression, argExpr: Expression) extends Expression
case class AdditionExpression(lhs: Expression, rhs: Expression) extends Expression
case class SubtractionExpression(lhs: Expression, rhs: Expression) extends Expression
case class MultiplicationExpression(lhs: Expression, rhs: Expression) extends Expression
case class DivisionExpression(lhs: Expression, rhs: Expression) extends Expression
case class ModuloExpression(lhs: Expression, rhs: Expression) extends Expression
case class BoxExpression(symbol: String, expr: Expression) extends Expression
case class IdExpression(symbol: String) extends Expression 

//case class ArithmeticExpression(op: String, lhs: Expression, rhs: Expression) extends Expression {
//    
//}
//case class OperatorExpression() extends Expression


/*
(define-type BCFAE-Value
(define-type Env
(define-type Store
(define-type ValuexStore
(define (num+ n1 n2) in Nothing
(define (env-lookup name env) in ENV
(define (store-lookup loc-index sto) in Store
(define (mod-store loc sto) in Store
(define (interp expr env store) in Program
(define next-location in Store


(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?) (rhs BCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BCFAE?)]
  [app (fun-expr BCFAE?) (arg-expr BCFAE?)]
  [if0 (test BCFAE?) (truth BCFAE?) (falsity BCFAE?)]
  )

(define-type BCFAE-Value 
  [numV (n number?)] 
  [closureV (param symbol?) (body BCFAE?) (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (location number?) (env Env?)])

(define-type Store
  [mtSto]
  [aSto (location number?) (value BCFAE-Value?) (store Store?)])

(define-type ValuexStore
  [vxs (value BCFAE-Value?) (store Store?)])

;; num+ : BCFAE-Value BCFAE-Value -> numV
(define (num+ n1 n2)
  (numV (+ (numV-n n1)
           (numV-n n2))))

;; num-zero? : BCFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

;; env-lookup : symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier")]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? bound-name name)
              bound-location
              (env-lookup name rest-env))]))

;; store-lookup : location Store -> BCFAE-Value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= location loc-index)
              value
              (store-lookup loc-index rest-store))]))

;; mod-store : location Store -> Store
(define (mod-store loc sto) 
  (type-case Store sto
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= location loc)
              (aSto loc value rest-store)
              (mod-store loc rest-store))]))

;; interp : BCFAE Env Store -> ValuexStore
(define (interp expr env store)
  (type-case BCFAE expr
    [num (n) (vxs (numV n) store)]
    [add (l r) (type-case ValuexStore (interp l env store)
                 [vxs (l-value l-store)
                      (type-case ValuexStore (interp r env l-store)
                        [vxs (r-value r-store)
                             (vxs (num+ l-value r-value)
                                  r-store)])])]
    [id (v) (vxs (store-lookup (env-lookup v env) store) store)]
    [fun (bound-id bound-body)
         (vxs (closureV bound-id bound-body env) store)]
    [app (fun-expr arg-expr)
         (type-case ValuexStore (interp fun-expr env store)
           [vxs (fun-value fun-store)
                (type-case ValuexStore (interp arg-expr env fun-store)
                  [vxs (arg-value arg-store)
                       (let ((new-loc (next-location arg-store)))
                         (interp (closureV-body fun-value)
                                 (aSub (closureV-param fun-value)
                                       new-loc
                                       (closureV-env fun-value))
                                 (aSto new-loc
                                       arg-value
                                       arg-store)))])])]
    [if0 (test truth falsity)
         (type-case ValuexStore (interp test env store)
           [vxs (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [newbox (value-expr)
            (type-case ValuexStore (interp value-expr env store)
              [vxs (expr-value expr-store)
                   (let ((new-loc (next-location expr-store)))
                     (vxs (boxV new-loc)
                          (aSto new-loc expr-value expr-store)))])]
    [setbox (box-expr value-expr)
            (type-case ValuexStore (interp box-expr env store)
              [vxs (box-value box-store)
                   (type-case ValuexStore (interp value-expr env box-store)
                     [vxs (value-value value-store)
                          (vxs value-value
                               (aSto (boxV-location box-value)
                                     value-value
                                     value-store))])])]
    [openbox (box-expr)
             (type-case ValuexStore (interp box-expr env store)
               [vxs (box-value box-store)
                    (vxs (store-lookup (boxV-location box-value)
                                       box-store)
                         box-store)])]
    [seqn (e1 e2)
          (type-case ValuexStore (interp e1 env store)
            [vxs (e1-value e1-store)
                 (interp e2 env e1-store)])]
    ))

;; next-location: Store -> location
(define next-location
  (local ([define last-loc (box -1)])
    (lambda (store)
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

(define (cyclically-bind-and-interp bound-id named-expr env)
(local ([define value-holder (box (numV 1729))]
[define new-env (aRecSub bound-id value-holder env)]
[define named-expr-val (interp named-expr new-env)])
(begin
(set-box! value-holder named-expr-val)
new-env))) 
 
*/