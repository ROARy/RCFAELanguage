package cs214.language

class ValueAndStore(val value : Value, val store : Store) extends Store {
    
    override def toString() : String = {
        "ValueAndStore(" + value + "," + store + ")" 
    }
}