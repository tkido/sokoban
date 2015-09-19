package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.{Map => MMap}
import com.tkido.tools.Log

class PullCounter(data:Data) {
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
  def countPull() :MMap[Int, MMap[Int, MMap[Int, Int]]] = {
    def getPullCounts(goal:Int): MMap[Int, MMap[Int, Int]] = {
      def checkHome(v:Int, bags:BitSet, checked:BitSet) :BitSet = {
        checked += v
        for (d <- neumann){
          if(!checked(v+d) && canMans(v+d) && !bags(v+d))
            checkHome(v+d, bags, checked)
        }
        checked
      }
      def check(v:Int, man:Int, distance:Int, direction:Int, maps:MMap[Int, MMap[Int, Int]]) :MMap[Int, MMap[Int, Int]] = {
        val homes = checkHome(man, BitSet(v), BitSet())
        maps(v)(direction) = distance
        for (d <- neumann)
          if (homes(v+d) && canBags(v+d) && canMans(v+d*2) &&
              distance+1 < maps(v+d)(d) &&
              !data.isDeadEnd(v+d*2, v+d) )
            check(v+d, v+d*2, distance+1, d, maps)
        maps
      }
      val maps = MMap[Int, MMap[Int, Int]]()
      canBags.toList.foreach(floor =>
        maps(floor) = MMap[Int, Int](-1 -> LARGEINT,
                                     1 -> LARGEINT,
                                     width -> LARGEINT,
                                     -width -> LARGEINT)
      )
      check(goal, initMan, 0, 0, maps)
    }
    val maps = MMap[Int, MMap[Int, MMap[Int, Int]]]()
    goals.toList.foreach(goal =>
      maps(goal) = getPullCounts(goal)
    )
    maps
  }
  
  Log d countPull
}

object PullCounter {
  def apply(data:Data) =
    new PullCounter(data)
}