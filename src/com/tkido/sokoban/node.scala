package com.tkido.sokoban

case class Node(
    id:BigInt,
    parent:Option[BigInt],
    count:Int,
    value:Int,
    sub:Boolean,
    var status:Int){
  
}
object Node{
  val UNKNOWN = 0
  val CHECKED = 1
  val LIVE = 3
  val DEAD = 4
}

case class Hand(from:Int, to:Int){
  def delta = to - from
  
  def direction :Int = {
    val del = delta
    val delAbs = del.abs
    if(delAbs >= Hand.width)
      del / (delAbs / Hand.width)
    else
      del / delAbs
  }
}
object Hand{
  var width = -1
}