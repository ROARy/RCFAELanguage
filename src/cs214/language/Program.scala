package cs214.language

import scala.collection.mutable.Queue
import java.util.logging.Logger
import java.util.logging.Level

/**
* May change name to Program
*/
class Program(rawScript: String) {
	private val LOGGER : Logger = Logger.getLogger(this.getClass().getSimpleName()) 
	private val DEBUG : Boolean = true
	//  private val AND = "&&"
	//  private val OR = "||"  
	//  private val NOT = "~"
	private val NUM_PATTERN = "(?<=\\{)(\\d+)(?=\\})".r
	private val APP1_PATTERN = "^\\{$".r
	private val APP2_PATTERN = "(?<=\\{)\\w+$".r
	private val IF0_PATTERN = "(?<=\\{)if0$".r
	private val FUN_PATTERN = "^\\{*fun\\}*$".r
	private val REC_PATTERN = "^\\{*rec\\}*$".r
	private val WITH_PATTERN = "^\\{*with\\}*$".r
	private val ADD_PATTERN = "^\\{*\\+\\}*$".r
	private val SUB_PATTERN = "^\\{*\\-\\}*$".r
	private val DIV_PATTERN = "^\\{*\\/\\}*$".r
	private val MULT_PATTERN = "^\\{*\\*\\}*$".r
	private val MOD_PATTERN = "^\\{*\\%\\}*$".r
	private val ID_PATTERN = "(?<=\\{)(\\w+)".r

	// Fields:
	var tokenQueue : Queue[String] = new Queue[String]
	var parsedProgram : Expression = _
	var interpretedProgram : Value = _
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
		val preProcessorPattern : String = "(?<=\\s|^)(\\w+)(?=\\s|\\}|$)"
		var orig : String = preProcessorPattern.r.replaceAllIn(rawScript, "{$1}").replaceAllLiterally(" ","")
		if (DEBUG) println(orig)

		// Split processed program into tokens.
		val splitPattern : String = "(?<!^)(?=\\{)"
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
			newExpression = new NumericExpression(num.get.toInt)
		} else if (IF0_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new ConditionalExpression(parse(), parse(), parse())
		} else if (FUN_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new FunctionExpression(parse(), parse())
		} else if (REC_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new RecursiveFunctionExpression(parse(), parse(), parse())
		} else if (WITH_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new ApplicationExpression(parse(), parse())
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
		} else if (APP1_PATTERN.findFirstIn(nextToken).isDefined) {
			newExpression = new ApplicationExpression(parse(), parse())
		} else if (app2.isDefined) {
			newExpression = new ApplicationExpression(new IdExpression(app2.get), parse())
		}else if (id.isDefined) {
			newExpression = new IdExpression(id.get)
		} else {
			throw new ParseException()
		}
		if (DEBUG) println(nextToken + ": " + newExpression.getClass().getSimpleName() + "(" + null + ")")
		return newExpression
	}

	// Used in testing.
	def interpretScript() : Value = {
		if (parsedProgram == null) {
			parsedProgram = parse()
		}
		if (DEBUG) println(parsedProgram)
		return interpret()
	}

	// .
	private def interpret() : Value = {
	    // TODO: Implement the interpret method.
	    
	    
		new Value()
	}

	// Used in testing.
	def evaluateScript() {
		if (interpretedProgram == null) {
			interpretedProgram = interpret()
		}
		return evaluate()
	}

	// .
	private def evaluate() {
	    // TODO: Make program return a simple value.
		return
	}

	/**
	* Tokenizes, parses, interprets and evaluates program, returning true upon success.
	*/
	def run() : Boolean = {
	    // TODO: Ensure that this runs properly.
		tokenQueue = tokenize()
		parsedProgram = parse()
		interpretedProgram = interpret()
		evaluatedProgram = evaluate()
		return true
	}

}