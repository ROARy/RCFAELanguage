package cs214.language

import java.util.logging.Logger
import scala.collection.mutable.Queue
import cs214.language.exceptions._

/**
* Takes a raw string input and can output the various stages of interpreting a 
* program. 
*/
class Program(var rawScript: String) {
	private val LOGGER : Logger = Logger.getLogger(this.getClass().getSimpleName()) 
	private val DEBUG : Boolean = false
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

	// Fields used in testing.
	var tokenQueue : Queue[String] = new Queue[String]
	var parsedProgram : Expression = _
	var interpretedProgram : ValueAndStore = _
	var evaluatedProgram : Any = _

	// Breaks program string into tokens based on regular expressions. Used in testing only.
	def tokenizeScript(rawString: String) : Queue[String] = {
		return tokenize(rawString)
	}

	// Breaks program string into tokens based on regular expressions.
	private def tokenize(rawString: String) : Queue[String] = {
		// Surround all expressions with braces and remove spaces.
		val preProcessorPattern : String = """(?<=\s|^)(\w+)(?=\s|\}|$)"""
		var orig : String = preProcessorPattern.r.replaceAllIn(rawString, "{$1}").replaceAllLiterally(" ","")

		// Split processed program into tokens.
		val splitPattern : String = """(?<!^)(?=\{)"""
		return new Queue[String] ++= orig.split(splitPattern)
	}

	// Accepts collection of tokens and matches keywords with their abstract representation. Used in testing only.
	def parseScript(rawString: String) : Expression = {
		tokenQueue = tokenize(rawString)
	    return parse()
	}

	// Accepts collection of tokens and matches keywords with their abstract representation.
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

		// TODO: Find a way to match
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

	// Accepts an expression as an argument and computes the result. Used in testing only.
	def interpretScript(expr : Expression) : ValueAndStore = {
		return interpret(expr, EmptyEnvironment(), EmptyStore())
	}

	// Accepts an expression as an argument and computes the result.
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
	        case AdditionExpression(lhs, rhs) => binaryOperation(lhs,rhs,env,sto,(x,y)=>x+y)
	        case SubtractionExpression(lhs, rhs) => binaryOperation(lhs,rhs,env,sto,(x,y)=>x-y)
	        case MultiplicationExpression(lhs, rhs) => binaryOperation(lhs,rhs,env,sto,(x,y)=>x*y)
	        case DivisionExpression(lhs, rhs) => binaryOperation(lhs,rhs,env,sto,(x,y)=>x/y)
	        case ModuloExpression(lhs, rhs) => binaryOperation(lhs,rhs,env,sto,(x,y)=>x%y)
	        case IdExpression(id) => new ValueAndStore(SimpleStore.lookup(SimpleEnvironment.lookup(id, env), sto), sto)
	        case _ => throw new InterpretException()
	    }
		return interpretation
	}
	
	// DRY way to handle different binary operations.
	private def binaryOperation(lhs: Expression, rhs: Expression, env: Environment, sto: Store, callback: (Int,Int) => Int) : ValueAndStore = {
	    val leftVxs = interpret(lhs, env, sto)
	    val leftArg = leftVxs.value match {
	    	case NumericValue(num) => num
	    }
	    val rightVxs = interpret(rhs, env, leftVxs.store)
	    val rightArg = rightVxs.value match {
	    	case NumericValue(num) => num
	    }
	    new ValueAndStore(NumericValue(callback(leftArg, rightArg)), rightVxs.store)
	}
	
	// Pretty-prints the result of running the program. Used in testing only.
	def evaluateScript(interpretation: ValueAndStore) {
		interpretedProgram = interpretation
	    evaluate(interpretedProgram)
	}

	// Pretty-prints the result of running the program.
	private def evaluate(vxs: ValueAndStore) {
	    val result = vxs.value match {
	        case NumericValue(num) => num
	        case ClosureValue(a,b,c) => ClosureValue(a,b,c)
	    }
	    println(result)
	}

	/**
	* Tokenizes, parses, interprets and evaluates program, returning true upon success.
	*/
	def run() : Boolean = {
	    // TODO: Ensure the run() method runs properly in Program class.
	    try {
	    	tokenQueue = tokenize(rawScript)
	    	if (DEBUG) tokenQueue.foreach(print _)
			parsedProgram = parse()
			if (DEBUG) println(parsedProgram)
			interpretedProgram = interpret(parsedProgram, EmptyEnvironment(), EmptyStore())
			if (DEBUG) println(interpretedProgram)
			evaluatedProgram = evaluate(interpretedProgram)
			if (DEBUG) println(evaluatedProgram)
			return true
	    } catch {
	        case te: TokenizeException => throw new TokenizeException()
	        case pe: ParseException => throw new ParseException()
	        case ie: InterpretException => throw new InterpretException()
	        case ee: EvaluateException => throw new EvaluateException()
	    }
	}
}