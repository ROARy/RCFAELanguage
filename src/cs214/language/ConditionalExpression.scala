package cs214.language

class ConditionalExpression(condition: Expression, truth: Expression, falsity: Expression) extends Expression {

    
    override def toString() : String = {
        "(ConditionalExpression " + condition + " " + truth + " " + falsity + ")"
    }
}