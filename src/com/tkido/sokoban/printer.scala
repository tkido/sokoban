package com.tkido.sokoban
import scala.collection.mutable.ArrayBuffer

class Printer(data:ProblemData) {
  private val writeMap = Map(
    FLOOR -> '　',
    GOAL  -> '・',
    MAN   -> '足',
    MONG  -> '足', //距
    AVOID -> '×',
    MONA  -> '足',
    BAG   -> '田',
    BONG  -> '回',
    WALL  -> '■',
    EXTRA -> '△',
    BLANK -> '　')
  
  val width = data.width
  val limit = data.limit
  
  def apply(data:ProblemData) :String = {
    val buf = new StringBuilder
    var i = 0
    while(i < limit){
      var x = data.naked(i)
      if(data.bags(i))
        x |= BAG
      else if(i == data.man)
        x |= MAN
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