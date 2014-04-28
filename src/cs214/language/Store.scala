package cs214.language

import cs214.language.exceptions.EmptyStoreException

abstract class Store
case class SimpleStore(location: Int, var value: Value, store: Store) extends Store
case class EmptyStore extends Store
object SimpleStore {
    def lookup(loc : Int, store: Store) : SimpleStore =  {
        store match {
            case SimpleStore(location, stoVal, sto) => {
                if (location != loc) {
		            SimpleStore.lookup(loc, sto)
		        } else {
		        	return SimpleStore(loc, stoVal, sto)
		        }
            }
            case EmptyStore() => throw new EmptyStoreException()
        }
    }
}