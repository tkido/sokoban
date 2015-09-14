package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object PullCounter{
  def apply(data:ProblemData) :Iterable[Array[Int]] = {
    val dec = DeadEndChecker(data)
    def getPullCount(goal:Int): Array[Int] = {
      val pullcount = Array.fill(data.limit)(Int.MaxValue)
      def check(v:Int, distance:Int){
        pullcount(v) = distance
        for (d <- data.neumann)
          if (data.naked(v+d) < WALL &&
              data.naked(v+d*2) < WALL &&
              distance+1 < pullcount(v+d) &&
              !dec.isDeadEnd(v+d*2, v+d) )
            check(v+d, distance+1)
      }
      check(goal, 0)
      Log i s"PullCounter pullcount:${pullcount.toList}"
      pullcount
    }
    data.goals.map(getPullCount)
    
  }
}
