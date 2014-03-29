package cs214.language

class RecursiveFunctionExpression(id: Expression, namedExpr: Expression, body: Expression) extends Expression {

    
    override def toString() : String = {
        "(RecursiveFunctionExpression " + id + " " + namedExpr + " " + body + ")"
    }
}