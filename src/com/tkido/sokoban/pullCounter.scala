package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

class PullCounter(naked:Array[Int]) {
  private val dec = DeadEndChecker(naked)
  val pullcounts = makePullCounts(naked)
  
  private def makePullCounts(naked:Array[Int]) :List[Array[Int]] = {
    def getPullCount(goal:Int): Array[Int] = {
      val pullcount = Array.fill(naked.size)(Int.MaxValue)
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


object PullCounter {
  def apply(naked:Array[Int]) = new PullCounter(naked)
}