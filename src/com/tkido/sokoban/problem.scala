package com.tkido.sokoban
import scala.collection.mutable.BitSet

case class ProblemInitialData(
    width:Int,
    height:Int,
    limit:Int,
    initMan:Int,
    initBags:BitSet,
    goals:BitSet,
    naked:Array[Int],
    neumann:List[Int]){
}
