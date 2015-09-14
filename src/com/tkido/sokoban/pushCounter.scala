package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object PushCounter{
  def apply(data:ProblemData) :Iterable[Array[Int]] = {
    def getPushCounts(goal:Int): Array[Int] = {
      def check(v:Int, distance:Int, counts:Array[Int]) :Array[Int] = {
        counts(v) = distance
        for (d <- data.neumann)
          if (data.canMans(v-d) &&
              data.canBags(v+d) &&
              distance+1 < counts(v+d))
            check(v+d, distance+1, counts)
        counts
      }
      check(goal, 0, Array.fill(data.limit)(LARGEINT))
    }
    data.bags.map(getPushCounts)
  }
}
