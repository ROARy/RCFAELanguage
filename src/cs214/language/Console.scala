package cs214.language

import java.util.regex._

object Console extends App {
    var program: Parser = _
    var str : String = "init"
    while (!str.isEmpty()) {
        print("~> ")
        str = readLine()
        program = new Parser(str)
        program.run()
        
        // {if0 {+ 37 4} {* {+ x 2} 1} {{fun {x} {+ 9 x}} 1}}
        // {+ 3 1}
        // {double 2}
        // {fun {x} {+ x 1}}
        // {{fun {x} {+ x 1}} 3}
    }   
}