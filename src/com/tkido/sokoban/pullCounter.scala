package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object PullCounter{
  def apply(data:ProblemData) :Iterable[Array[Int]] = {
    val dec = DeadEndChecker(data)
    def getPullCounts(goal:Int): Array[Int] = {
      def check(v:Int, distance:Int, counts:Array[Int]) :Array[Int] = {
        counts(v) = distance
        for (d <- data.neumann)
          if (data.canMans(v+d) &&
              data.canMans(v+d*2) &&
              distance+1 < counts(v+d) &&
              !dec.isDeadEnd(v+d*2, v+d) )
            check(v+d, distance+1, counts)
        counts
      }
      check(goal, 0, Array.fill(data.limit)(Int.MaxValue))
    }
    data.goals.map(getPullCounts)
  }
}
