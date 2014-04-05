package cs214.language

import cs214.language.exceptions.EmptyEnvironmentException

abstract class Environment
case class SimpleEnvironment(symbol: String, location: Int, env: Environment) extends Environment
case class RecursiveEnvironment(symbol: String, location: Int, env: Environment) extends Environment
case class EmptyEnvironment extends Environment
object SimpleEnvironment {
    def lookup(symbol : String, env: Environment) : Int =  {
        env match {
            case SimpleEnvironment(sym, location, environ) => {
                if (symbol == sym) {
		            return location
		        } else {
		        	SimpleEnvironment.lookup(sym, environ)
		        }
            }
            case EmptyEnvironment() => throw new EmptyEnvironmentException()
        }
    }
}