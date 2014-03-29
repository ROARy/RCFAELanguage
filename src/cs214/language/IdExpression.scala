package cs214.language

class IdExpression(val name: String) extends Expression {

    // TODO: Make sure characters provided are valid identifiers.
    private def isValidId() : Boolean = {
        true
    }
    
	override def toString() : String = {
		"(IdExpression '" + name + ")"
	}
}