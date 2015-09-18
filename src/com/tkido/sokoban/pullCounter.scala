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
  
  /**
   * Pull a imaginary BAG from each GOAL with counting distance
   */
  def countPull() :MMap[MMap[Int]] = {
    def getPullCounts(goal:Int): Array[Int] = {
      def check(v:Int, distance:Int, direction:Int, maps:MMap[Int, MMap[Int, Int]]) :MMap[Int, MMap[Int, Int]]) = {
        maps(v)(direction) = distance
        for (d <- neumann)
          if (canBags(v+d) && canMans(v+d*2) &&
              distance+1 < maps(v+d)(d) &&
              !data.isDeadEnd(v+d*2, v+d) )
            check(v+d, distance+1, counts)
        counts
      }
      check(goal, 0, Array.fill(limit)(LARGEINT))
    }
    goals.toList.map(getPullCounts)
  }
}

object PullCounter {
  def apply(data:Data) =
    new PullCounter(data)
}