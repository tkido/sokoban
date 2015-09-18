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
      else if(i == man)
        x |= MAN
      buf += writeMap(x)
      i += 1
      if ((i % width) == 0)
        buf += '\n'
    }
    buf += '★'
    buf.toString
  }
  def apply(data:Data) :String =
    apply(data.man, data.bags)
  def apply(id:BigInt) :String = {
    val (man, bags) = ider.fromId(id)
    apply(man, bags)
  }
  def apply(node:Node) :String = {
    val depth = if(node.sub) "sub" else "MAIN"
    val header = s"${depth} push:${node.count} value:${node.value} id:${node.id} parent:${node.parent}\n"
    val body = node.parent match{
      case None => apply(node.id)
      case Some(parentId) =>
        val bags = ider.fromId(node.id)._2
        val lastBags = ider.fromId(parentId)._2
        val lastBag = (lastBags &~ bags).head
        apply(lastBag, bags)
    }
    header + body
  }
}

object Printer {
  def apply(data:Data) =
    new Printer(data)
}