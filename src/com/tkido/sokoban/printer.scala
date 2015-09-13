package com.tkido.sokoban

class Printer(width:Int) {
  def apply(arr:Array[Int]) :String = {
    val buf = new StringBuilder
    val limit = arr.size
    var i = 0
    while(i < limit){
      buf += writeMap(arr(i))
      i += 1
      if ((i % width) == 0)
        buf += '\n'
    }
    buf.toString
  }
}

object Printer {
  def apply(width:Int) =
    new Printer(width:Int)
}