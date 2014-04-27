package cs214.language.tests

import cs214.language._
import org.scalatest._

class InterpreterTest extends FunSuite with BeforeAndAfter {

    val program1 = new Program("{3}")
    val program2 = new Program("78")
    val program3 = new Program("{+ 3 2}")
    val program4 = new Program("{+ {* 1 7} 4}")
    val program5 = new Program("{fun {x} {+ 3 x}}")
    val program6 = new Program("{if0 {+ 3 4} {fun {x} {* x 2}} 8}")
    val program7 = new Program("{double 3}")
    val program8 = new Program("{double {if0 {0} 1 {2}}}")
    val program9 = new Program("x")
    val program10 = new Program("{rec {x 3} {+ 3 4}}")
    
    before {
        
    }
    
//    test("this") {
//        
//    }
    
    test("interpet() properly reduces expressions into value stores") {
//        assert("ValueAndStore(NumericValue(3),EmptyStore())" === interpretScript(NumericValue(3)).toString)
//        assert("ValueAndStore(NumericValue(78),EmptyStore())" === program2.interpretScript.toString)
//        assert("ValueAndStore(NumericValue(5),EmptyStore())" === program3.interpretScript.toString)
//        assert("ValueAndStore(NumericValue(11),EmptyStore())" === program4.interpretScript.toString)
//        assert("ValueAndStore(NumericValue(ClosuerValue(x,AdditionExpression(IdExpression(x),NumericExpression(3)),EmptyEnvironment()),EmptyStore())" === program5.interpretScript.toString)
//        assert("ValueAndStore(ClosureValue(x,MultiplicationExpression(IdExpression(x),NumericExpression(2)),EmptyEnvironment()),EmptyStore())" === program6.interpretScript.toString)
//        assert("ValueAndStore(ClosureValue(),EmptyStore())" === program7.interpretScript.toString) // Won't work.
//        assert("ValueAndStore(NumericValue(11),EmptyStore())" === program8.interpretScript.toString) // Won't work.
//        assert("ValueAndStore(NumericValue(11),EmptyStore())" === program9.interpretScript.toString) // Won't work.
//        assert("ValueAndStore(NumericValue(11),EmptyStore())" === program10.interpretScript.toString) // Doesn't work yet.
    }
}