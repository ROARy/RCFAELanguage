package cs214.language.tests

import cs214.language._
import org.scalatest._

class TokenizerTest extends FunSuite with BeforeAndAfter {
    
    val program1 = new Program("{3}")
    val program2 = new Program("78")
    val program3 = new Program("{+ 3 2}")
    val program4 = new Program("{+ {* 1 7} 4}")
    val program5 = new Program("{fun {x} {+ 3 x}}")
    val program6 = new Program("{if0 {+ 3 4} {fun {x} {* x 2}} {+ 8 8}}")
    val program7 = new Program("{double 3}")
    val program8 = new Program("{double {if0 {+ 3 3} {+ 1 1} {+ 2 2}}}")
    val program9 = new Program("x")
    val program10 = new Program("{rec {x 3} {+ 3 4}}")
    
    before {
        
    }
    
    
    test("tokenize method properly splits a program into the correct number of tokens.") {
        assert(1 === program1.tokenizeScript.length)
        assert(1 === program2.tokenizeScript.length)
        assert(3 === program3.tokenizeScript.length)
        assert(5 === program4.tokenizeScript.length)
        assert(5 === program5.tokenizeScript.length)
        assert(12 === program6.tokenizeScript.length)
        assert(2 === program7.tokenizeScript.length)
        assert(11 === program8.tokenizeScript.length)
        assert(1 === program9.tokenizeScript.length)
        assert(6 === program10.tokenizeScript.length)
    }
    
    test("tokenize method properly handles error.") {
        
    }
}