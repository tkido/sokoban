package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack

case class Data(
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

case class Node(
    id:BigInt,
    parent:Option[BigInt],
    count:Int,
    value:Int,
    var status:Int){
  
}
object Node{
  val UNKNOWN = 0
  val CHECKED = 1
  val LIVE = 3
  val DEAD = 4
}