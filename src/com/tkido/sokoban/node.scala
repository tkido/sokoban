package com.tkido.sokoban

case class Node(
    id:BigInt,
    parent:Option[BigInt],
    count:Int,
    value:Int,
    sub:Boolean,
    lastHand:Option[Hand],
    var status:Int){
  
}
object Node{
  val UNKNOWN = 0
  val CHECKED = 1
  val LIVE = 3
  val DEAD = 4
}
