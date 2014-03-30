package cs214.language

class DivisionExpression(lhs: Expression, rhs: Expression) extends ArithmeticExpression(lhs: Expression, rhs: Expression) {

	override def toString() : String = {
		"(DivisionExpression " + lhs + " " + rhs + ")"
	}
}