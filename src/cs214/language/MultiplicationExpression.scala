package cs214.language

class MultiplicationExpression(lhs: Expression, rhs: Expression) extends ArithmeticExpression(lhs: Expression, rhs: Expression) {

	override def toString() : String = {
		"(MultiplicationExpression " + lhs + " " + rhs + ")"
	}  
}