package cs214.language

/**
 * Object represents an interactive REPL console where you can test out the RCFAELanguage. 
 */
object Console extends App {
    var program: Program = _

    var str : String = "empty"
    
    while (!str.isEmpty()) {
        print("~> ")
        str = readLine()
        program = new Program(str)
        
        program.run()
    }
}