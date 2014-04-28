package cs214.language.tests

import cs214.language._
import org.scalatest._

class TokenizerTest extends FunSuite with BeforeAndAfter {
    
	val program = new Program("")
    
    test("Tokenizes a program into the correct number of tokens.") {
        assert(1 === program.tokenizeScript("{3}").length)
        assert(1 === program.tokenizeScript("78").length)
        assert(3 === program.tokenizeScript("{+ 3 2}").length)
        assert(5 === program.tokenizeScript("{+ {* 1 7} 4}").length)
        assert(5 === program.tokenizeScript("{fun {x} {+ 3 x}}").length)
        assert(12 === program.tokenizeScript("{if0 {+ 3 4} {fun {x} {* x 2}} {+ 8 8}}").length)
        assert(2 === program.tokenizeScript("{double 3}").length)
        assert(11 === program.tokenizeScript("{double {if0 {+ 3 3} {+ 1 1} {+ 2 2}}}").length)
        assert(1 === program.tokenizeScript("x").length)
        assert(17 === program.tokenizeScript("{rec fact {fun {n} {if0 {- n 1} 1 {* n {fact {- n 1}}}}} {fact 4}}").length)
    }
}