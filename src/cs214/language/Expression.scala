package cs214.language

/*
 * 
 * <RCFAE> ::=   <num>
 * 				| {+ <RCFAE> <RCFAE>}
 *     			| {* <RCFAE> <RCFAE>}
 *        		| <id>
 *          	| {fun {<id>} <RCFAE>}
 *           	| {<RCFAE> <RCFAE>}
 *            	| {if0 <RCFAE> <RCFAE> <RCFAE>}
 *             	| {rec {<id>} {<RCFAE>} <RCFAE>}
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
case class IdExpression(symbol: String) extends Expression