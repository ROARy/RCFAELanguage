package cs214.language

import java.util.regex._


object Console extends App {
	
    var program: Parser = _
    var str : String = "init"
    while (!str.isEmpty()) {
        print("~> ")
        str = readLine()
        program = new Parser(str)
        program.superRun()
        println(program.superTokenize)
        println("Abstract Syntax: " + program.parsedProgram)
        
//        println(program.superTokenize())
        // {if0 {+ 37 4} {- {+ x 2} 1} {{fun {x} {+ 9 x}} 1}}
    }
}