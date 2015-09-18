package com.tkido.sokoban

case class Hand(from:Int, to:Int, width:Int){
  def delta = to - from
  
  def direction :Int = {
    val del = delta
    val delAbs = del.abs
    if(delAbs >= width)
      del / (delAbs / width)
    else
      del / delAbs
  }
  
  def size :Int = {
    val delAbs = delta.abs
    if(delAbs >= width)
      delAbs / width
    else
      delAbs
  }
  
  override def toString :String =
    "(%d->%d)".format(from, to)
}


class HandFactory(width:Int){
  def apply(from:Int, to:Int) = Hand(from, to, width)
}
object HandFactory{
  def apply(width:Int) = new HandFactory(width)
}