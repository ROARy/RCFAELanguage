package cs214.language

class ArithmeticExpression(lhs: Expression, rhs: Expression) extends Expression {
  
  
  override def toString() : String = {
      "(ArithmeticExpression " + lhs + " " + rhs + ")"
  }
    
}
//case class AdditionExpression(leftArg: Int, rightArg: Int) extends ArithmeticExpression {
//  
//}
//case class SubtractionExpression(leftArg: Int, rightArg: Int) extends ArithmeticExpression
//case class MultiplicationExpression(leftArg: Int, rightArg: Int) extends ArithmeticExpression
//case class DivisionExpression(leftArg: Int, rightArg: Int) extends ArithmeticExpression
//case class ModuloExpression(leftArg: Int, rightArg: Int) extends ArithmeticExpression
