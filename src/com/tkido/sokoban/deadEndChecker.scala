package com.tkido.sokoban
import scala.collection.mutable.BitSet

class DeadEndChecker(width:Int, naked:Array[Int]) {
  //val NEUMANN = List(-width, -1, 1, width)
  
  def getHomeSize(man:Int, bags:BitSet) :Int = {
    val checked = BitSet()
    def check(v:Int){
      checked += v
      for (d <- NEUMANN)
        if (!checked(v+d) && (naked(v+d) < 5) && !bags(v+d))
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
  def apply(width:Int, naked:Array[Int]) =
    new DeadEndChecker(width, naked)
}