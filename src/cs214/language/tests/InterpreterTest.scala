package cs214.language.tests

import cs214.language._
import org.scalatest._

class InterpreterTest extends FunSuite with BeforeAndAfter {

    val program = new Program("")
    
    test("Interprets numeric expressions properly.") {
        assert("ValueAndStore(NumericValue(3),EmptyStore())" === program.interpretScript("3").toString())
        assert("ValueAndStore(NumericValue(1729),EmptyStore())" === program.interpretScript("1729").toString())
        assert("ValueAndStore(NumericValue(1000000),EmptyStore())" === program.interpretScript("{1000000}").toString())
    }
    
    test("Interprets conditional expressions properly.") {
        assert("ValueAndStore(NumericValue(1),EmptyStore())" === program.interpretScript("{if0 0 1 2}").toString())
        assert("ValueAndStore(NumericValue(5),EmptyStore())" === program.interpretScript("{if0 {+ 1 0} {+ 5 5} {- 10 5}}").toString())
    }
    
    test("Interprets function expressions properly.") {
        assert("ValueAndStore(ClosureValue(x,AdditionExpression(IdExpression(x),NumericExpression(1)),EmptyEnvironment()),EmptyStore())" === program.interpretScript("{fun {x} {+ x 1}}").toString())
        assert("ValueAndStore(ClosureValue(x,ConditionalExpression(IdExpression(x),NumericExpression(1),NumericExpression(2)),EmptyEnvironment()),EmptyStore())" === program.interpretScript("{fun {x} {if0 x 1 2}}").toString())
        assert("ValueAndStore(ClosureValue(x,NumericExpression(3),EmptyEnvironment()),EmptyStore())" === program.interpretScript("{fun {x} 3}").toString())
    }
    
    test("Interprets recursive function applications properly.") {
        assert("ValueAndStore(NumericValue(0),SimpleStore(6,NumericValue(0),SimpleStore(5,NumericValue(1),SimpleStore(4,NumericValue(2),SimpleStore(3,NumericValue(3),SimpleStore(2,NumericValue(4),SimpleStore(1,NumericValue(5),SimpleStore(0,ClosureValue(x,ConditionalExpression(SubtractionExpression(IdExpression(x),NumericExpression(0)),NumericExpression(0),ApplicationExpression(IdExpression(reduceToZero),SubtractionExpression(IdExpression(x),NumericExpression(1)))),SimpleEnvironment(reduceToZero,0,EmptyEnvironment())),EmptyStore()))))))))"
                === program.interpretScript("{rec {reduceToZero} {fun {x} {if0 {- x 0} 0 {reduceToZero {- x 1}}}} {reduceToZero 5}}").toString())
        assert("ValueAndStore(NumericValue(6),SimpleStore(4,NumericValue(0),SimpleStore(3,NumericValue(1),SimpleStore(2,NumericValue(2),SimpleStore(1,NumericValue(3),SimpleStore(0,ClosureValue(n,ConditionalExpression(SubtractionExpression(IdExpression(n),NumericExpression(0)),NumericExpression(1),MultiplicationExpression(IdExpression(n),ApplicationExpression(IdExpression(fact),SubtractionExpression(IdExpression(n),NumericExpression(1))))),SimpleEnvironment(fact,0,EmptyEnvironment())),EmptyStore()))))))" 
                === program.interpretScript("{rec {fact} {fun {n} {if0 {- n 0} 1 {* n {fact {- n 1}}}} {fact 3}}").toString())
        assert("ValueAndStore(NumericValue(720),SimpleStore(7,NumericValue(0),SimpleStore(6,NumericValue(1),SimpleStore(5,NumericValue(2),SimpleStore(4,NumericValue(3),SimpleStore(3,NumericValue(4),SimpleStore(2,NumericValue(5),SimpleStore(1,NumericValue(6),SimpleStore(0,ClosureValue(n,ConditionalExpression(SubtractionExpression(IdExpression(n),NumericExpression(0)),NumericExpression(1),MultiplicationExpression(IdExpression(n),ApplicationExpression(IdExpression(fact),SubtractionExpression(IdExpression(n),NumericExpression(1))))),SimpleEnvironment(fact,0,EmptyEnvironment())),EmptyStore())))))))))" 
                === program.interpretScript("{rec {fact} {fun {n} {if0 {- n 0} 1 {* n {fact {- n 1}}}} {fact 6}}").toString())
    }
    
    test("Interprets symbols and applications properly.") {
        assert("ValueAndStore(NumericValue(6),SimpleStore(0,NumericValue(3),EmptyStore()))" 
                === program.interpretScript("{{fun {x} {+ x x}} 3}").toString())
        assert("ValueAndStore(NumericValue(20),SimpleStore(1,NumericValue(10),SimpleStore(0,ClosureValue(x,AdditionExpression(IdExpression(x),IdExpression(x)),EmptyEnvironment()),EmptyStore())))" 
                === program.interpretScript("{{fun {func} {func 10}} {fun {x} {+ x x}}}").toString())
    }
    
    test("Interprets binary arithmetic expressions properly.") {
        assert("ValueAndStore(NumericValue(3),EmptyStore())" === program.interpretScript("{+ 1 2}").toString())
        assert("ValueAndStore(NumericValue(10),EmptyStore())" === program.interpretScript("{+ {+ 1 2} {+ 3 4}}").toString())
        
        assert("ValueAndStore(NumericValue(5),EmptyStore())" === program.interpretScript("{- 10 5}").toString())
        assert("ValueAndStore(NumericValue(0),EmptyStore())" === program.interpretScript("{- {- 4 3} {- 2 1}}").toString())
        
        assert("ValueAndStore(NumericValue(50),EmptyStore())" === program.interpretScript("{* 5 10}").toString())
        assert("ValueAndStore(NumericValue(120),EmptyStore())" === program.interpretScript("{* {* 2 3} {* 4 5}}").toString())
        
        assert("ValueAndStore(NumericValue(2),EmptyStore())" === program.interpretScript("{/ 20 10}").toString())
        assert("ValueAndStore(NumericValue(1),EmptyStore())" === program.interpretScript("{/ {/ 20 10} {/ 100 50}}").toString())
        
        assert("ValueAndStore(NumericValue(1),EmptyStore())" === program.interpretScript("{% 5 4}").toString())
        assert("ValueAndStore(NumericValue(1),EmptyStore())" === program.interpretScript("{% {% 9 5} {% 13 10}}").toString())
    }
}