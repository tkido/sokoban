package com.tkido.sokoban
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.BitSet

class Printer(data:Data) {
  private val ider = Identifier(data)
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
  
  private val width = data.width
  private val limit = data.limit
  private val naked = data.naked
  
  def apply(man:Int, bags:BitSet) :String = {
    val buf = new StringBuilder
    var i = 0
    while(i < limit){
      var x = naked(i)
      if(bags(i))
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
  def apply(data:Data) :String =
    apply(data.man, data.bags)
  def apply(id:BigInt) :String = {
    val (man, bags) = ider.fromId(id)
    apply(man, bags)
  }
  def apply(node:Node) :String = {
    s"ID:${node.id} parent:${node.parent} count:${node.count} value:${node.value} status:${node.status}\n" +
    apply(node.id)
  }
}

object Printer {
  def apply(data:Data) =
    new Printer(data)
}