package cs214.language

import java.util.regex._

object Console extends App {
    var program: Program = _
    var str : String = "init"
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