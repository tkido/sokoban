package com.tkido.sokoban

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
  
  override def toString :String =
    "(%d->%d)".format(from, to)
}
object Hand{
  var width = -1
}
