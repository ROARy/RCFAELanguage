package cs214.language

import java.util.regex._

object Console extends App {
    var program: Program = _
    var str : String = "3"
    (new Program(str)).run()
    
    str = "{+ 3 4}"
    (new Program(str)).run()
    
    str = "{+ 1 {+ {+ 3 10} 2}}"
    (new Program(str)).run()
    
    str = "{{fun {x} {+ x 1}} 9}"
    (new Program(str)).run()
    
    str = "{fun {x} 3}"
    (new Program(str)).run()
    
    str = "{if0 0 1 2}"
    (new Program(str)).run()
    
    str = "{if0 {- 1 1} {{fun {x} {/ x 10}} 100} {* 1 2}}"
    (new Program(str)).run()
    
    str = "{with {x 2} {+ x 1}}"
    (new Program(str)).run()
        
    while (!str.isEmpty()) {
        print("~> ")
        str = readLine()
        program = new Program(str)
        try {
            program.run()
        } catch {
            case ex : ParseException => println("ERROR! ~ Too few arguments. Try again.")
        }
    }   
}