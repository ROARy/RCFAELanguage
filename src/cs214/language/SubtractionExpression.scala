package cs214.language

class SubtractionExpression(lhs: Expression, rhs: Expression) extends ArithmeticExpression(lhs: Expression, rhs: Expression) {

  override def toString() : String = {
      "(SubtractionExpression " + lhs + " " + rhs + ")"
  }
}