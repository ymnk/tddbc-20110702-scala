package lru

import java.util.Date
import scala.collection.mutable.{Map, ListBuffer}

class Lru[K, V](private[this] var cacheSize:Int = 2) extends Removable {
    
    private[this] val map = Map.empty[K, V] 
    private[this] val keystack = new KeyStack
    
    def put(key: K, value: V): Unit = {
        keystack.touch(key)
        map += key -> value
    }
    
    def get(key: K): Option[V] = map.get(key) map { v =>
        keystack.touch(key)
        v
    }
    
    private[lru] def peek(key: K): Option[V] = map.get(key)
    
    def changeSize(size: Int): Unit = keystack.changeSize(size)

    def remove(time:Date): Unit = keystack.removeBefore(time)
    
    private class KeyStack {
        
        val stack = ListBuffer.empty[(K, Date)]

        def touch(key: K) = {
            stack.find(_._1 == key) map { 
              stack -= _ 
            } getOrElse(stack) += ( key -> new Date )
            shrinkIfNeeded()
        }
        
        def changeSize(size: Int) = {
            cacheSize = size
            shrinkIfNeeded()
        }
        
        private def shrinkIfNeeded() = if (stack.size > cacheSize) {
            drop(stack.take( stack.size - cacheSize ))
        }

        def removeBefore(time:Date) =
            drop(stack.takeWhile{ ! _._2.after(time) })

        private def drop(e:Traversable[(K, _)]) = {
          e.foreach{ e => map.remove(e._1) }
          stack.trimStart(e.size)
        }
    }
}
