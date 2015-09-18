package com.tkido.sokoban

case class Hand(from:Int, to:Int, width:Int){
  def delta = to - from
  
  def direction :Int =
    delta.signum * (if(delta.abs < width) 1 else width)
  
  def size :Int =
    delta.abs match{
      case n if(n < width) => n
      case n => n / width
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