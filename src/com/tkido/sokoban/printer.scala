package com.tkido.sokoban

class Printer(data:ProblemData) {
  val width = data.width
  val limit = data.limit
  
  def apply(data:ProblemData) :String = {
    val buf = new StringBuilder
    var i = 0
    while(i < limit){
      val x = i match{
        case data.man => data.naked(i) | MAN
        case n if data.bags(i) => data.naked(i) | BAG
        case _ => data.naked(i)
      }
      buf += writeMap(x)
      i += 1
      if ((i % width) == 0)
        buf += '\n'
    }
    buf.toString
  }
}

object Printer {
  def apply(data:ProblemData) =
    new Printer(data)
}