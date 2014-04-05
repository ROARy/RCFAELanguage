package cs214.language

abstract class Value
case class NumericValue(num: Int) extends Value
case class ClosureValue(symbol: String, body: Expression, env: Environment) extends Value