package cs214.language

class FunctionExpression(name: Expression, value: Expression) extends Expression {

    override def toString() : String = {
        "(FunctionExpression " + name + " " + value + ")"
    }
}