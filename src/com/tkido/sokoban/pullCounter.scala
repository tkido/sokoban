package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object PullCounter{
  def apply(data:ProblemInitialData) :List[Array[Int]] = {
    val dec = DeadEndChecker(data)
    val naked = data.naked
    def getPullCount(goal:Int): Array[Int] = {
      val pullcount = Array.fill(data.limit)(Int.MaxValue)
      def check(v:Int, distance:Int):Unit = {
        pullcount(v) = distance
        for (d <- NEUMANN)
          if (naked(v+d) < WALL &&
              naked(v+d*2) < WALL &&
              distance+1 < pullcount(v+d) &&
              !dec.isDeadEnd(v+d*2, v+d) )
            check(v+d, distance+1)
      }
      check(goal, 0)
      Log i s"PullCounter pullcount:${pullcount.toList}"
      pullcount
    }
    val goals = BitSet()
    naked.zipWithIndex.foreach{case(n, i) =>
       if((n & GOAL) == GOAL) goals += i
    }
    goals.toList.map(getPullCount)
    
  }
}
