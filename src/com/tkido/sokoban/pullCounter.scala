package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object PullCounter{
  def apply(data:ProblemData) :Iterable[Array[Int]] = {
    val dec = DeadEndChecker(data)
    def getPullCount(goal:Int): Array[Int] = {
      def check(v:Int, distance:Int, count:Array[Int]) :Array[Int] = {
        count(v) = distance
        for (d <- data.neumann)
          if (data.naked(v+d) < WALL &&
              data.naked(v+d*2) < WALL &&
              distance+1 < count(v+d) &&
              !dec.isDeadEnd(v+d*2, v+d) )
            check(v+d, distance+1, count)
        count
      }
      check(goal, 0, Array.fill(data.limit)(Int.MaxValue))
    }
    data.goals.map(getPullCount)
  }
}
