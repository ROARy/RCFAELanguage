package cs214.language

class ModuloExpression(lhs: Expression, rhs: Expression) extends ArithmeticExpression(lhs: Expression, rhs: Expression) {

	override def toString() : String = {
		"(ModuloExpression " + lhs + " " + rhs + ")"
	}  
}