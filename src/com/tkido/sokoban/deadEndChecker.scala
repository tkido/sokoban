package com.tkido.sokoban
import scala.collection.mutable.BitSet

class DeadEndChecker(data:ProblemInitialData) {
  def getHomeSize(man:Int, bags:BitSet) :Int = {
    val checked = BitSet()
    def check(v:Int){
      checked += v
      for (d <- data.neumann)
        if (!checked(v+d) && (data.naked(v+d) < 5) && !bags(v+d))
          check(v+d)
    }
    check(man)
    checked.size
  }
  def isDeadEnd(man:Int, bags:BitSet) :Boolean =
    getHomeSize(man, bags) < 5
  
  def getHomeSize(man:Int, bag:Int) :Int =
    getHomeSize(man, BitSet(bag))
  def isDeadEnd(man:Int, bag:Int) :Boolean =
    isDeadEnd(man, BitSet(bag))
}
object DeadEndChecker{
  def apply(data:ProblemInitialData) =
    new DeadEndChecker(data)
}