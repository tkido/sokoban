package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.{Map => MMap}
import com.tkido.tools.Log

class DirectionalPullCounter(data:Data) {
  val goals = data.goals
  val neumann = data.neumann
  val canMans = data.canMans
  val canBags = data.canBags
  val limit = data.limit
  val width = data.width
  val initMan = data.initMan
  
  /**
   * Pull a imaginary BAG from each GOAL with counting distance
   */
  def countPull() :Map[Int, Map[Int, MMap[Int, Int]]] = {
    def getPullCounts(goal:Int): Map[Int, MMap[Int, Int]] = {
      def checkHome(v:Int, bags:BitSet, checked:BitSet) :BitSet = {
        checked += v
        for (d <- neumann){
          if(!checked(v+d) && canMans(v+d) && !bags(v+d))
            checkHome(v+d, bags, checked)
        }
        checked
      }
      def check(v:Int, man:Int, distance:Int, direction:Int, maps:Map[Int, MMap[Int, Int]]) :Map[Int, MMap[Int, Int]] = {
        val homes = checkHome(man, BitSet(v), BitSet())
        maps(v)(direction) = distance
        for (d <- neumann)
          if (homes(v+d) && canBags(v+d) && canMans(v+d*2) &&
              (!maps(v+d).contains(d) || distance+1 < maps(v+d)(d)) &&
              !data.isDeadEnd(v+d*2, v+d) )
            check(v+d, v+d*2, distance+1, d, maps)
        maps
      }
      val maps = canBags.toList.map(_ -> MMap[Int, Int]()).toMap
      check(goal, initMan, 0, 0, maps)
    }
    goals.toList.map(g => g -> getPullCounts(g)).toMap
  }
  
  val pullCounts = countPull()
  Log d pullCounts
}

object DirectionalPullCounter {
  def apply(data:Data) =
    new DirectionalPullCounter(data)
}