package cs214.language


import scala.collection.mutable.Queue
import java.util.logging.Logger
import java.util.logging.Level

/**
 * May change name to Program
 */
class Parser(rawScript: String) {

  private val LOGGER : Logger = Logger.getLogger(this.getClass().getSimpleName()) 
  private val DEBUG : Boolean = true
  
  private val LEFT_BRACE = "{"
  private val RIGHT_BRACE = "}"
  private val PLUS = "+"
  private val MULT = "*"
  private val IF0 = "if0"
  private val FUN = "fun"
  private val REC = "rec"
  private val WITH = "with"
//  private val MINUS = "-"
//  private val DIV = "/"
//  private val MOD = "%"
//  private val AND = "&&"
//  private val OR = "||"  
//  private val NOT = "~"
  
  private val NUM_PATTERN = "\\{*(\\d+)\\}*"
  private val APP_PATTERN = "{"
  private val IF0_PATTERN = "\\{*if0\\}*"
  private val FUN_PATTERN = "\\{*fun\\}*"
  private val REC_PATTERN = "\\{*rec\\}*"
  private val WITH_PATTERN = "\\{*with\\}*"
  private val PLUS_PATTERN = "\\{*\\+\\}*"
  private val MULT_PATTERN = "\\{*\\*\\}*"
      
  // Fields:
  var tokens : Queue[String] = new Queue[String]
  var parsedProgram : Expression = _
  var interpretedProgram : Int = _
  
  /**
   *  Simple method to manipulate and tokenize the input program.
   */ 
  def tokenize() : Queue[String] = {
	val regex = "(?<=\\{)|(?=\\})|\\s+" // Split on spaces or braces
	return new Queue[String] ++= rawScript.trim().split(regex)
  }
  
  /**
   *  Employs more complex regex to manipulate and tokenize the input program.
   */ 
  def superTokenize() : Queue[String] = {
	// TODO: combine patterns 1 & 2
	val pattern1 : String = "\\s+(\\w+)\\s+"			// Surrounds expressions with braces.
    val pattern2 : String = "(?<=\\})\\s*(\\w+)(?=\\})"	// Finishes surrounding with braces.
//    val pattern5 : String = "(?<!^)(?=\\{)|(?<=\\})(?!$)" // Used to split string into tokens.
    val pattern5 : String = "(?<!^)(?=\\{)" // Used to split string into tokens.
    
    // Process string and split in to tokens.
    var orig : String = pattern1.r.replaceAllIn(rawScript, "{$1}")
    orig = pattern2.r.replaceAllIn(orig, "{$1}").replaceAllLiterally(" ","")
    
    return new Queue[String] ++= orig.split(pattern5)
  }
  
  def parseScript() : Expression = {
      if (tokens.isEmpty) {
          tokens = superTokenize()
      }
      return parse()
  }
  
  private def parse() : Expression = {
      if (DEBUG) println(tokens)
      if (tokens.isEmpty) {
          throw new ParseException() // Exception("Tried to parse empty program.")
      }
      
      var nextToken : String = tokens.dequeue
      var newExpression : Expression = null
      var allElseFails : Boolean = true    
      
      if (DEBUG) println("Current token: " + nextToken)
      
      // Check for arithmetic, conditional, function, recursion.
      // TODO: Order conditionals so that the likelier choices are closer to the top.
      if (nextToken.equals(LEFT_BRACE)) {
          if (DEBUG) println("nextToken <" + nextToken + "> is LEFT_BRACE")
          nextToken = tokens.dequeue
          if (nextToken.equals(IF0)) {
              if (DEBUG) println("nextToken <" + nextToken + "> is IF0..creating ConditionalExpression")
              val condition : Expression = parse()
              val truth : Expression = parse()
              val falsity : Expression = parse()
              newExpression = new ConditionalExpression(condition, truth, falsity)
              //newExpression = new ConditionalExpression(parse(), parse(), parse()) // Quicker.
          } else if (nextToken.equals(WITH)) {
              if (DEBUG) println("nextToken <" + nextToken + "> is WITH..creating ApplicationExpression")
              val function : Expression = parse()
              val args : Expression = parse()
              newExpression = new ApplicationExpression(function, args)
              //newExpression = new ApplicationExpression(parse(), parse())
          } else if (nextToken.equals(FUN)) {
              if (DEBUG) println("nextToken <" + nextToken + "> is FUN..creating FunctionExpression")
              val id : Expression = parse()
              val body : Expression = parse()
              newExpression = new FunctionExpression(id, body)
              //newExpression = new FunctionExpression(parse(), parse())
          } else if (nextToken.equals(REC)) {
              if (DEBUG) println("nextToken <" + nextToken + "> is REC..creating RecusriveFunctionExpression")
              val id : Expression = parse()
              val namedExpr : Expression = parse()
              val body : Expression = parse()
              newExpression = new RecursiveFunctionExpression(id, namedExpr, body)
              //newExpression = new RecursiveFunctionExpression(parse(), parse(), parse())
          } else if (nextToken.equals(PLUS)) {
              if (DEBUG) println("nextToken <" + nextToken + "> is PLUS..creating ArithmeticExpression")
              val lhs : Expression = parse()
              val rhs : Expression = parse()
              newExpression = new ArithmeticExpression(lhs, rhs)
              //newExpression = new ArithmeticExpression(parse(), parse())
          } else if (nextToken.equals(MULT)) {
              if (DEBUG) println("nextToken <" + nextToken + "> is MULT..creating ArithmeticExpression")
              val lhs : Expression = parse()
              val rhs : Expression = parse()
              newExpression = new ArithmeticExpression(lhs, rhs)
              //newExpression = new ArithmeticExpression(parse(), parse())
          } else if (nextToken.equals(LEFT_BRACE)) {
              if (DEBUG) println("nextToken <" + nextToken + "> is LEFT_BRACE..creating ApplicationExpression")
              val func : Expression = parse()
              val args : Expression = parse()
              newExpression = new ApplicationExpression(func, args)
              //newExpression = new ApplicationExpression(parse(), parse())
          } else {
              if (DEBUG) println("nextToken <" + nextToken + "> is ID..creating ApplicationExpression")
              val func : Expression = new IdExpression(nextToken)
              val args : Expression = parse()
              newExpression = new ApplicationExpression(func, args)
              //newExpression = new ApplicationExpression(new IdExpression(nextToken), parce())
          }
          if (!tokens.dequeue.equals(RIGHT_BRACE)) {
        	  if (DEBUG) println("nextToken <" + nextToken + "> is not a right brace")
        	  throw new ParseException()
          }
      } else {
	      // Check for a number.
	      try {
	          val number: Int = nextToken.toInt
	          newExpression = new NumericExpression(number)
	          if (DEBUG) println("nextToken <" + nextToken + "> is number")          
	      } catch {
	          case ex: Exception => newExpression = new IdExpression(nextToken)
	      }
      }
      return newExpression
  }
  
  def superParse() : Expression = {
      var newExpression : Expression = null
      
      // Ensure everything is as it should be.
      
      // Grab the next token.
      val nextToken = tokens.dequeue
      
      // Do the matching.
      newExpression = nextToken match {
          case NUM_PATTERN => {
              var result = NUM_PATTERN.r.replaceAllIn(nextToken,"$1")
              println("Result: " + result)
              new NumericExpression(result.toInt)
          }
          case APP_PATTERN => {
              new ApplicationExpression(parse(), parse())
          }
          case IF0_PATTERN => {
              new ConditionalExpression(parse(), parse(), parse())
          }
          case FUN_PATTERN => {
              new FunctionExpression(parse(), parse())
          }
          case REC_PATTERN => {
              new RecursiveFunctionExpression(parse(), parse(), parse())
          }
          case WITH_PATTERN => {
              new ApplicationExpression(parse(), parse())
          }
          case PLUS_PATTERN => {
              new ArithmeticExpression(parse(), parse())
          }
          case MULT_PATTERN => {
              new ArithmeticExpression(parse(), parse())
          }
          case _ => throw new ParseException()
      }
      
      return newExpression
  }
  
  def interpret() : Expression = {
    if (parsedProgram == null) {
      parsedProgram = parse()
    }
    
    new NumericExpression(4)
  }
  
  /**
   * Helper method that will tokenize, parse, interpret and evaluate program.
   */
  def superRun() : Boolean = {
    tokens = superTokenize()
    parsedProgram = superParse()
    true
//    interpretedProgram = interpret()
//    evaluatedProgram = evaluate()
  }
  
}