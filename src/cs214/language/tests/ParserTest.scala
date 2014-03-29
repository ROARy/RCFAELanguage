package cs214.language.tests

import cs214.language._

class ParserTest {

  def testNumericExpression = assert("(NumericExpression 3)" == (new NumericExpression(3)).toString)
  
}
