package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack

case class ProblemData(
    width:Int,
    height:Int,
    limit:Int,
    var man:Int,
    bags:BitSet,
    goals:BitSet,
    canMans:BitSet,
    canBags:BitSet,
    naked:Array[Int],
    neumann:List[Int],
    steps:Stack[Int]){
}
