package com.tkido.sokoban
import scala.collection.mutable.BitSet

case class ProblemInitialData(
    width:Int,
    height:Int,
    limit:Int,
    man:Int,
    bags:BitSet,
    goals:BitSet,
    canMans:BitSet,
    canBags:BitSet,
    naked:Array[Int],
    neumann:List[Int]){
}
