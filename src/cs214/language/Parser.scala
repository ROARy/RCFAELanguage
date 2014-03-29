package cs214.language


import scala.collection.mutable.Queue

import java.util.logging.Logger
import java.util.logging.Level

/**
 * May change name to Program
 */
class Parser(rawScript: String) {

  private val LOGGER : Logger = Logger.getLogger(this.getClass().getSimpleName()) 
  private val DEBUG : Boolean = false
  
  private val LEFT_BRACE = "{"
  private val RIGHT_BRACE = "}"
//  private val MINUS = "-"
//  private val DIV = "/"
//  private val MOD = "%"
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
  private val PLUS_PATTERN = "^\\{*\\+\\}*$".r
  private val MULT_PATTERN = "^\\{*\\*\\}*$".r
  private val ID_PATTERN = "(?<=\\{)(\\w+)".r
      
  // Fields:
  var tokens : Queue[String] = new Queue[String]
  var parsedProgram : Expression = _
  var interpretedProgram : Value = _
  
  /**
   *  Employs more complex regex to manipulate and tokenize the input program.
   */ 
  def tokenize() : Queue[String] = {
	// TODO: combine patterns 1 & 2
	val pattern1 : String = "(?<=\\s)(\\w+)(?=\\s)"			// Surrounds expressions with braces.
    val pattern2 : String = "(?<=\\s)(\\w+)(?=\\})"	// Finishes surrounding with braces.
    val pattern3 : String = "(?<!^)(?=\\{)" // Used to split string into tokens.
    
    // Process string and split in to tokens.
    var orig : String = pattern1.r.replaceAllIn(rawScript, "{$1}")
    println(orig)
    orig = pattern2.r.replaceAllIn(orig, "{$1}").replaceAllLiterally(" ","")
    println(orig)
    return new Queue[String] ++= orig.split(pattern3)
  }
  
  def parseScript() : Expression = {
      if (tokens.isEmpty) {
          tokens = tokenize()
      }
      return parse()
  }
  
  private def parse() : Expression = {
      var newExpression : Expression = null
      
      // Grab the next token.
      val nextToken = tokens.dequeue
      
      // Try to match patterns to the token.
      // TODO: There has to be a better way to do this but I can't find one.
      val num = NUM_PATTERN.findFirstIn(nextToken)
      val if0 = IF0_PATTERN.findFirstIn(nextToken)
      val fun = FUN_PATTERN.findFirstIn(nextToken)
      val rec = REC_PATTERN.findFirstIn(nextToken)
      val let = WITH_PATTERN.findFirstIn(nextToken)
      val plus = PLUS_PATTERN.findFirstIn(nextToken)
      val mult = MULT_PATTERN.findFirstIn(nextToken)
      val app1 = APP1_PATTERN.findFirstIn(nextToken)
      val app2 = APP2_PATTERN.findFirstIn(nextToken)
      val id = ID_PATTERN.findFirstIn(nextToken)
      
      // Match.
      if (num.isDefined) {
          newExpression = new NumericExpression(num.get.toInt)
      } else if (if0.isDefined) {
          newExpression = new ConditionalExpression(parse(), parse(), parse())
      } else if (fun.isDefined) {
          newExpression = new FunctionExpression(parse(), parse())
      } else if (rec.isDefined) {
          newExpression = new RecursiveFunctionExpression(parse(), parse(), parse())
      } else if (let.isDefined) {
          newExpression = new ApplicationExpression(parse(), parse())
      } else if (plus.isDefined) {
          newExpression = new ArithmeticExpression(parse(), parse())
      } else if (mult.isDefined) {
          newExpression = new ArithmeticExpression(parse(), parse())
      } else if (app1.isDefined) {
          newExpression = new ApplicationExpression(parse(), parse())
  	  } else if (app2.isDefined) {
          newExpression = new ApplicationExpression(new IdExpression(app2.get), parse())
  	  }else if (id.isDefined) {
          newExpression = new IdExpression(id.get)
      } else {
          if (DEBUG) println(nextToken + ": not found.")
//          throw new ParseException()
      }
      
      if (DEBUG) println(nextToken + ": " + newExpression.getClass().getSimpleName() + "(" + null + ")")
      
      return newExpression
  }
  
  def interpret() : Value = {
    if (parsedProgram == null) {
      parsedProgram = parse()
    }
    
    println(parsedProgram)
    
    new Value()
  }
  
  /**
   * Helper method that will tokenize, parse, interpret and evaluate program.
   */
  def run() : Boolean = {
    tokens = tokenize()
    parsedProgram = parse()
    interpretedProgram = interpret()
    true
//    evaluatedProgram = evaluate()
  }
  
}