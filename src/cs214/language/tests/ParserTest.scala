package cs214.language.tests

import cs214.language._
import org.scalatest._

class ParserTest extends FunSuite with BeforeAndAfter {
  
    val program = new Program("")
    
    test("Parses numbers properly.") {
        assert("NumericExpression(3)" === program.parseScript("3").toString())
        assert("NumericExpression(1729)" === program.parseScript("1729").toString())
        assert("NumericExpression(1000000)" === program.parseScript("{1000000}").toString())
    }
    
    test("Parses conditional statements properly.") {
        assert("ConditionalExpression(NumericExpression(0),NumericExpression(1),NumericExpression(2))" === program.parseScript("{if0 0 1 2}").toString())
        assert("ConditionalExpression(AdditionExpression(NumericExpression(1),NumericExpression(0)),AdditionExpression(NumericExpression(5),NumericExpression(5)),SubtractionExpression(NumericExpression(10),NumericExpression(5)))" === program.parseScript("{if0 {+ 1 0} {+ 5 5} {- 10 5}}").toString())
    }
    
    test("Parses function definitions properly.") {
        assert("FunctionExpression(x,AdditionExpression(IdExpression(x),NumericExpression(1)))" === program.parseScript("{fun {x} {+ x 1}}").toString())
        assert("FunctionExpression(x,ConditionalExpression(IdExpression(x),NumericExpression(1),NumericExpression(2)))" === program.parseScript("{fun {x} {if0 x 1 2}}").toString())
        assert("FunctionExpression(x,NumericExpression(3))" === program.parseScript("{fun {x} 3}").toString())
    }
    
    test("Parses recursive definitions and applications properly.") {
        assert("RecursiveFunctionExpression(reduceToZero,FunctionExpression(x,ConditionalExpression(SubtractionExpression(IdExpression(x),NumericExpression(0)),NumericExpression(0),ApplicationExpression(IdExpression(reduceToZero),SubtractionExpression(IdExpression(x),NumericExpression(1))))),ApplicationExpression(IdExpression(reduceToZero),NumericExpression(10)))" 
                === program.parseScript("{rec {reduceToZero} {fun {x} {if0 {- x 0} 0 {reduceToZero {- x 1}}}} {reduceToZero 10}}").toString())
        assert("RecursiveFunctionExpression(fact,FunctionExpression(n,ConditionalExpression(SubtractionExpression(IdExpression(n),NumericExpression(0)),NumericExpression(1),MultiplicationExpression(IdExpression(n),ApplicationExpression(IdExpression(fact),SubtractionExpression(IdExpression(n),NumericExpression(1)))))),ApplicationExpression(IdExpression(fact),NumericExpression(5)))" 
                === program.parseScript("{rec {fact} {fun {n} {if0 {- n 0} 1 {* n {fact {- n 1}}}} {fact 5}}").toString())
        assert("RecursiveFunctionExpression(fact,FunctionExpression(n,ConditionalExpression(SubtractionExpression(IdExpression(n),NumericExpression(0)),NumericExpression(1),MultiplicationExpression(IdExpression(n),ApplicationExpression(IdExpression(fact),SubtractionExpression(IdExpression(n),NumericExpression(1)))))),ApplicationExpression(IdExpression(fact),NumericExpression(10)))" 
                === program.parseScript("{rec {fact} {fun {n} {if0 {- n 0} 1 {* n {fact {- n 1}}}} {fact 10}}").toString())
    }
    
    test("Parses applications properly.") {
        assert("ApplicationExpression(IdExpression(double),NumericExpression(3))" === program.parseScript("{double 3}").toString())
        assert("ApplicationExpression(FunctionExpression(x,AdditionExpression(IdExpression(x),IdExpression(x))),NumericExpression(3))" === program.parseScript("{{fun {x} {+ x x}} 3}").toString())
        assert("ApplicationExpression(FunctionExpression(func,ApplicationExpression(IdExpression(func),NumericExpression(10))),FunctionExpression(x,AdditionExpression(IdExpression(x),IdExpression(x))))" === program.parseScript("{{fun {func} {func 10}} {fun {x} {+ x x}}}").toString())
    }
    
    test("Parses binary arithmetic operations properly.") {
        assert("AdditionExpression(NumericExpression(1),NumericExpression(2))" === program.parseScript("{+ 1 2}").toString())
        assert("AdditionExpression(AdditionExpression(NumericExpression(1),NumericExpression(2)),AdditionExpression(NumericExpression(3),NumericExpression(4)))" === program.parseScript("{+ {+ 1 2} {+ 3 4}}").toString())
        
        assert("SubtractionExpression(NumericExpression(10),NumericExpression(5))" === program.parseScript("{- 10 5}").toString())
        assert("SubtractionExpression(SubtractionExpression(NumericExpression(4),NumericExpression(3)),SubtractionExpression(NumericExpression(2),NumericExpression(1)))" === program.parseScript("{- {- 4 3} {- 2 1}}").toString())
        
        assert("MultiplicationExpression(NumericExpression(5),NumericExpression(10))" === program.parseScript("{* 5 10}").toString())
        assert("MultiplicationExpression(MultiplicationExpression(NumericExpression(2),NumericExpression(3)),MultiplicationExpression(NumericExpression(4),NumericExpression(5)))" === program.parseScript("{* {* 2 3} {* 4 5}}").toString())
        
        assert("DivisionExpression(NumericExpression(20),NumericExpression(10))" === program.parseScript("{/ 20 10}").toString())
        assert("DivisionExpression(DivisionExpression(NumericExpression(20),NumericExpression(10)),DivisionExpression(NumericExpression(100),NumericExpression(50)))" === program.parseScript("{/ {/ 20 10} {/ 100 50}}").toString())
        
        assert("ModuloExpression(NumericExpression(5),NumericExpression(4))" === program.parseScript("{% 5 4}").toString())
        assert("ModuloExpression(ModuloExpression(NumericExpression(9),NumericExpression(5)),ModuloExpression(NumericExpression(13),NumericExpression(10)))" === program.parseScript("{% {% 9 5} {% 13 10}}").toString())
    }
    
    test("Parses symbols properly.") {
        assert("IdExpression(x)" === program.parseScript("x").toString())
        assert("IdExpression(hello)" === program.parseScript("{hello}").toString())
        assert("ApplicationExpression(IdExpression(double),IdExpression(triple))" === program.parseScript("{double triple}").toString())
    }
}
