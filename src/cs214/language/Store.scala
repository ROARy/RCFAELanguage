package cs214.language

abstract class Store
case class SimpleStore(location: Int, value: Value, store: Store) extends Store
case class EmptyStore extends Store
case class RecursiveStore extends Store
object SimpleStore {
    def lookup(loc : Int, store: Store) : Value =  {
        store match {
            case SimpleStore(location, stoVal, sto) => {
                if (location == loc) {
		            return stoVal
		        } else {
		        	SimpleStore.lookup(loc, sto)
		        }
            }
            case EmptyStore() => throw new EmptyStoreException()
        }
    }
}