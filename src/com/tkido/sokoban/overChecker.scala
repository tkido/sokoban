package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.{Map => MMap}
import com.tkido.tools.Log

class OverChecker(data:Data) {
  val bagsSize = data.bags.size
  val naked = data.naked
  
  val pullables = data.pullCounts.map(_.map(c => if(c < LARGEINT) 1 else 0))
  val pullableSum = pullables
    .fold(Array.fill(data.limit)(0))((a1, a2) => (a1 zip a2).map(p => p._1 + p._2))
  Log d s"OverChecker pullableSum:${pullableSum.toList}"
  
  val limits = MMap[Int, Int]()
  val cpMap = MMap[Int, BitSet]()
  
  pullableSum.zipWithIndex.foreach{case(c, v) =>
    if(0 < c && c < bagsSize){
      val checkPoints = makeCheckPoints(c, v)
      if(c < checkPoints.size){
        limits(v) = c
        cpMap(v) = checkPoints
        Log d s"limit: ${c}\ncheckPoints: ${checkPoints}"
      }
    }
  }
  Log i s"limits: ${limits}\ncpMap: ${cpMap}"
  
  private def makeCheckPoints(limit:Int, point:Int) :BitSet = {
    def check(v:Int, checked:BitSet) :BitSet = {
      checked += v
      for (d <- data.neumann)
        if (!checked(v+d) &&
            pullableSum(v+d) <= limit &&
            data.canMans(v-d) &&
            data.canBags(v+d) &&
            !data.isDeadEnd(v+d, v) )
          check(v+d, checked)
      checked
    }
    check(point, BitSet())
  }
  
  def apply(v:Int, bags:BitSet) :Boolean = {
    if(limits.contains(v))
      limits(v) < (cpMap(v) & bags).size
    else
      false
  }
}


object OverChecker {
  def apply(data:Data) =
    new OverChecker(data)
}