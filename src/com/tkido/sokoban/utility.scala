package com.tkido.sokoban

class Utility(naked:Board) {
  import scala.collection.mutable.{Set => MSet}
  import scala.collection.mutable.{Map => MMap}
  
  def getHomeSize(man:Vector, bags:Set[Vector]) :Int = {
    val checked = MSet[Vector]()
    def check(v:Vector){
      checked += v
      for (d <- NEUMANN)
        if (!checked(v+d) && naked.canMan(v+d) && !bags(v+d))
          check(v+d)
    }
    check(man)
    checked.size
  }
  def isDeadEnd(man:Vector, bags:Set[Vector]) :Boolean =
    getHomeSize(man, bags) < 5
  
  def getHomeSize(man:Vector, bag:Vector) :Int =
    getHomeSize(man, Set(bag))
  def isDeadEnd(man:Vector, bag:Vector) :Boolean =
    isDeadEnd(man, Set(bag))
}


object Utility {
  def apply(naked:Board) = new Utility(naked)
}