package cs214.language

class NumericExpression(val num: Int) extends Expression {
    
	override def toString() : String = {
	  "(NumericExpression " + num + ")"
	}
}