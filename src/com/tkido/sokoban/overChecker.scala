package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.{Map => MMap}
import com.tkido.tools.Log

class OverChecker(data:Data) {
  val bagsSize = data.bags.size
  
  val pullables = data.pullCounts.map(_.map{
    case LARGEINT => 0
    case _ => 1
  })
  val limits = pullables
    .fold(Array.fill(data.limit)(0))((a1, a2) => (a1 zip a2).map(p => p._1 + p._2))  
  val limitMap = MMap[Int, Int]()
  val areaMap = MMap[Int, BitSet]()
  
  data.canBags.foreach{v =>
    val limit = limits(v)
    if(0 < limit && limit < bagsSize){
      val checkArea = makeCheckArea(limit, v)
      if(limit < checkArea.size){
        limitMap(v) = limit
        areaMap(v) = checkArea
      }
    }
  }
  Log i s"limitMap: ${limitMap}\nareaMap: ${areaMap}"
  
  private def makeCheckArea(limit:Int, v:Int) :BitSet = {
    def check(v:Int, checked:BitSet) :BitSet = {
      checked += v
      for (d <- data.neumann)
        if (!checked(v+d) &&
            limits(v+d) <= limit &&
            data.canMans(v-d) &&
            data.canBags(v+d) &&
            !data.isDeadEnd(v+d, v) )
          check(v+d, checked)
      checked
    }
    check(v, BitSet())
  }
  
  def apply(v:Int, bags:BitSet) :Boolean = {
    if(limitMap.contains(v))
      limitMap(v) < (areaMap(v) & bags).size
    else
      false
  }
}


object OverChecker {
  def apply(data:Data) =
    new OverChecker(data)
}