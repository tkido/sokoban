package com.tkido.sokoban
import scala.collection.mutable.BitSet

class DeadEndChecker(data:Data) {
  def getHomeSize(man:Int, bags:BitSet) :Int = {
    def check(v:Int, done:BitSet) :BitSet = {
      done += v
      for (d <- data.neumann)
        if (!done(v+d) && data.canMans(v+d) && !bags(v+d))
          check(v+d,done)
      done
    }
    check(man, BitSet()).size
  }
  def isDeadEnd(man:Int, bags:BitSet) :Boolean =
    getHomeSize(man, bags) < 5
  
  def getHomeSize(man:Int, bag:Int) :Int =
    getHomeSize(man, BitSet(bag))
  def isDeadEnd(man:Int, bag:Int) :Boolean =
    isDeadEnd(man, BitSet(bag))
}
object DeadEndChecker{
  def apply(data:Data) =
    new DeadEndChecker(data)
}