package cs214.language.tests

import cs214.language._
import org.scalatest._

class ParserTest extends FunSuite with BeforeAndAfter {
  
    val program1 = new Program("{3}")
    val program2 = new Program("78")
    val program3 = new Program("{+ 3 2}")
    val program4 = new Program("{+ {* 1 7} 4}")
    val program5 = new Program("{fun {x} {+ 3 x}}")
    val program6 = new Program("{if0 {+ 3 4} {fun {x} {* x 2}} {+ 8 8}}")
    val program7 = new Program("{double 3}")
    val program8 = new Program("{double {if0 {0} {1} {2}}}")
    val program9 = new Program("x")
    val program10 = new Program("{rec {x 3} {+ 3 4}}")
    
    before {
        
    }
    
    test("parse method properly parses program into abstract syntax") {
        assert("(NumericExpression 3)" === program1.parseScript.toString)
        assert("(NumericExpression 78)" === program2.parseScript)
        assert("(ArithmeticExpression (NumericExpression 3) (NumericExpression 2))" === program3.parseScript)
        assert("(ArithmeticExpression (ArithmeticExpression (NumericExpression 1) (NumericExpression 7)) (NumericExpression 4))" === program4.parseScript)
        assert("(FunctionExpression (IdExpression 'x) (ArithmeticExpression (NumericExpression 3) (IdExpression 'x)))" === program5.parseScript)
        assert("(NumericExpression (NumericExpression 3) (NumericExpression 3) (NumericExpression 3))" === program6.parseScript)
        assert("(ApplicationExpression (IdExpression 'double) (NumericExpression 3))" === program7.parseScript)
        assert("(ApplicationExpression (IdExpression 'double) (ConditionalExpression (NumericExpression 0) (NumericExpression 1) (NumericExpression 2)))" === program8.parseScript)
        assert("(IdExpression 'x)" === program9.parseScript)
        assert("(RecursiveFunctionExpression (IdExpression 'x) (NumericExpression 3) (ArithmeticExpression (NumericExpression 3) (NumericExpression 4)))" === program10.parseScript)
    }
}
