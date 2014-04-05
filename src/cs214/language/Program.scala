package cs214.language

import java.util.logging.Logger

import scala.collection.mutable.Queue

/**
* May change name to Program
*/
class Program(rawScript: String) {
	private val LOGGER : Logger = Logger.getLogger(this.getClass().getSimpleName()) 
	private val DEBUG : Boolean = true
	//  private val AND = "&&"
	//  private val OR = "||"  
	//  private val NOT = "~"
	private val NUM_PATTERN = """(?<=\{)(\d+)(?=\})""".r
	private val APP1_PATTERN = """^\{$""".r
	private val APP2_PATTERN = """(?<=\{)\w+$""".r
	private val IF0_PATTERN = """(?<=\{)if0$""".r
	private val FUN_PATTERN = """^\{*fun$""".r
	private val REC_PATTERN = """^\{*rec$""".r
	private val WITH_PATTERN = """^\{*with$""".r
	private val ADD_PATTERN = """^\{*\+$""".r
	private val SUB_PATTERN = """^\{*\-$""".r
	private val DIV_PATTERN = """^\{*\/$""".r
	private val MULT_PATTERN = """^\{*\*$""".r
	private val MOD_PATTERN = """^\{*\%$""".r
	private val ID_PATTERN = """(?<=\{)(\w+)""".r
	private val BOX_PATTERN = """^\{*box$""".r

	// Fields:
	var tokenQueue : Queue[String] = new Queue[String]
	var parsedProgram : Expression = _
	var interpretedProgram : ValueAndStore = _
	var evaluatedProgram : Any = _

	// Used in testing.
	def tokenizeScript : Queue[String] = {
		if (rawScript == null) {
			throw new ParseException()
		}
		return tokenize()
	}

	// .
	private def tokenize() : Queue[String] = {
		// Surround all expressions with braces and remove spaces.
		val preProcessorPattern : String = """(?<=\s|^)(\w+)(?=\s|\}|$)"""
		var orig : String = preProcessorPattern.r.replaceAllIn(rawScript, "{$1}").replaceAllLiterally(" ","")

		// Split processed program into tokens.
		val splitPattern : String = """(?<!^)(?=\{)"""
		return new Queue[String] ++= orig.split(splitPattern)
	}

	// Used in testing.
	def parseScript() : Expression = {
		if (tokenQueue.isEmpty) {
			tokenQueue = tokenize()
		}
		return parse()
	}

	// .
	private def parse() : Expression = {	    
		// Grab the next token.
		if (tokenQueue.isEmpty) {
			throw new ParseException()
		} 
		val nextToken = tokenQueue.dequeue
		var newExpression : Expression = null

		// Try to match patterns to the token.
		// TODO: There has to be a better way to do this but I can't find one.
		val num = NUM_PATTERN.findFirstIn(nextToken)
		val app2 = APP2_PATTERN.findFirstIn(nextToken)
		val id = ID_PATTERN.findFirstIn(nextToken)

		// Match.
		if (num.isDefined) {
			newExpression = NumericExpression(num.get.toInt)
		} else if (IF0_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = ConditionalExpression(parse(), parse(), parse())
		} else if (FUN_PATTERN.findFirstIn(nextToken).isDefined) {
		    var idExp : IdExpression = parse() match {case IdExpression(x) => IdExpression(x)}
			newExpression = FunctionExpression(idExp.symbol, parse())
		} else if (REC_PATTERN.findFirstIn(nextToken).isDefined) {
		    var idExp : IdExpression = parse() match {case IdExpression(x) => IdExpression(x)}
			newExpression = RecursiveFunctionExpression(idExp.symbol, parse(), parse())
		} else if (WITH_PATTERN.findFirstIn(nextToken).isDefined) {
		    if (true) {} // TODO: need some extra stuff here.
			newExpression = ApplicationExpression(parse(), parse())
		} else if (ADD_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new AdditionExpression(parse(), parse())
		} else if (SUB_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new SubtractionExpression(parse(), parse())
		} else if (MULT_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new MultiplicationExpression(parse(), parse())
		} else if (DIV_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new DivisionExpression(parse(), parse())
		} else if (MOD_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new ModuloExpression(parse(), parse())
		} else if (BOX_PATTERN.findFirstIn(nextToken).isDefined) {
		    var idExp : IdExpression = parse() match {case IdExpression(x) => IdExpression(x)}
			newExpression = new BoxExpression(idExp.symbol, parse())
		} else if (APP1_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new ApplicationExpression(parse(), parse())
		} else if (app2.isDefined) {
			newExpression = new ApplicationExpression(new IdExpression(app2.get), parse())
		} else if (id.isDefined) {
			newExpression = new IdExpression(id.get)
		} else {
			throw new ParseException()
		}
		return newExpression
	}

	// Used in testing.
	def interpretScript() : ValueAndStore = {
		if (parsedProgram == null) {
			parsedProgram = parse()
		}
		return interpret(parsedProgram, EmptyEnvironment(), EmptyStore())
	}

	// .
	private def interpret(expr : Expression, env : Environment, sto : Store) : ValueAndStore = {
	    // TODO: Implement the interpret method in Program class.
	    var interpretation : ValueAndStore = expr match {
	        case NumericExpression(num) => new ValueAndStore(NumericValue(num), sto)
	        case ConditionalExpression(test, truth, falsity) => {
	            var nextVxs = interpret(test, env, sto)
	            val result = nextVxs.value match {
	                case NumericValue(num) => num 
	                case _ => throw new InterpretException()
	            }
	            // Use newly computed ValueStore's store to interpret the truth or falsity branch based on condition.
	            nextVxs = interpret(if (result == 0) truth else falsity, env, nextVxs.store)
	            new ValueAndStore(nextVxs.value, nextVxs.store)
	        }
	        case FunctionExpression(symbol, body) => new ValueAndStore(ClosureValue(symbol, body, env), sto)
	        case RecursiveFunctionExpression(symbol, expr, body) => {
	            // TODO: Need special implementation steps included in RecursiveFunctionExpression section of interpret() in Program class.
	            throw new InterpretException()
	            new ValueAndStore(ClosureValue(symbol, body, env), sto)
	        }
	        case ApplicationExpression(funExpr, argExpr) => {
	            val funVxs = interpret(funExpr, env, sto) 
	            val argVxs = interpret(argExpr, env, funVxs.store)
	            val closure = funVxs.value match {
	                case ClosureValue(param, body, env) => ClosureValue(param, body, env)
	                case _ => throw new InterpretException()
	            }
	            val currentLocation = argVxs.store match {
	                case SimpleStore(loc, _, _) => loc
	                case EmptyStore() => -1
	                case _ => throw new InterpretException()
	            }
	            val nextLocation = currentLocation + 1
	            interpret(closure.body, 
	                    new SimpleEnvironment(closure.symbol, nextLocation, closure.env), 
	                    new SimpleStore(nextLocation, argVxs.value, argVxs.store))
	        }
	        case AdditionExpression(lhs, rhs) => { 
	            val leftVxs = interpret(lhs, env, sto)
	            val leftArg = leftVxs.value match {
	                case NumericValue(num) => num
	            }
	            val rightVxs = interpret(rhs, env, leftVxs.store)
	            val rightArg = rightVxs.value match {
	                case NumericValue(num) => num
	            }
	            new ValueAndStore(NumericValue(leftArg + rightArg), rightVxs.store)
	        }
	        case SubtractionExpression(lhs, rhs) => { 
	            val leftVxs = interpret(lhs, env, sto)
	            val leftArg = leftVxs.value match {
	                case NumericValue(num) => num
	            }
	            val rightVxs = interpret(rhs, env, leftVxs.store)
	            val rightArg = rightVxs.value match {
	                case NumericValue(num) => num
	            }
	            new ValueAndStore(NumericValue(leftArg - rightArg), rightVxs.store)
	        }
	        case MultiplicationExpression(lhs, rhs) => { 
	            val leftVxs = interpret(lhs, env, sto)
	            val leftArg = leftVxs.value match {
	                case NumericValue(num) => num
	            }
	            val rightVxs = interpret(rhs, env, leftVxs.store)
	            val rightArg = rightVxs.value match {
	                case NumericValue(num) => num
	            }
	            new ValueAndStore(NumericValue(leftArg * rightArg), rightVxs.store)
	        }
	        case DivisionExpression(lhs, rhs) => { 
	            val leftVxs = interpret(lhs, env, sto)
	            val leftArg = leftVxs.value match {
	                case NumericValue(num) => num
	            }
	            val rightVxs = interpret(rhs, env, leftVxs.store)
	            val rightArg = rightVxs.value match {
	                case NumericValue(num) => num
	            }
	            new ValueAndStore(NumericValue(leftArg / rightArg), rightVxs.store)
	        }
	        case ModuloExpression(lhs, rhs) => { 
	            val leftVxs = interpret(lhs, env, sto)
	            val leftArg = leftVxs.value match {
	                case NumericValue(num) => num
	            }
	            val rightVxs = interpret(rhs, env, leftVxs.store)
	            val rightArg = rightVxs.value match {
	                case NumericValue(num) => num
	            }
	            new ValueAndStore(NumericValue(leftArg % rightArg), rightVxs.store)
	        }
	        // case BoxExpression(box) => new ValueStore(, sto)
	        case IdExpression(id) => new ValueAndStore(SimpleStore.lookup(SimpleEnvironment.lookup(id, env), sto), sto)
	        case _ => throw new InterpretException()
	    }
	    
		return interpretation
	}
	
	// Used in testing.
	def evaluateScript() {
		if (interpretedProgram == null) {
			interpretedProgram = interpret(parsedProgram, EmptyEnvironment(), EmptyStore())
		}
		return evaluate()
	}

	// .
	private def evaluate() : Any = {
	    val result = interpretedProgram.value match {
	        case NumericValue(num) => num
	        case ClosureValue(a,b,c) => ClosureValue(a,b,c)
	    }
	    return result
	}

	/**
	* Tokenizes, parses, interprets and evaluates program, returning true upon success.
	*/
	def run() : Boolean = {
	    // TODO: Ensure the run() method runs properly in Program class.
	    try {
	    	tokenQueue = tokenize()
			parsedProgram = parse()
			interpretedProgram = interpret(parsedProgram, EmptyEnvironment(), EmptyStore())
			evaluatedProgram = evaluate()
			return true
	    } catch {
	        case e => throw new ParseException()
	    }
	}
}