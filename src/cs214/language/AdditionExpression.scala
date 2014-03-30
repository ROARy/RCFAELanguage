package cs214.language

class AdditionExpression(lhs: Expression, rhs: Expression) extends ArithmeticExpression(lhs: Expression, rhs: Expression) {

	override def toString() : String = {
		"(AdditionExpression " + lhs + " " + rhs + ")"
	}
}