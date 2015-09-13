package com.tkido.sokoban

import scala.collection.mutable.BitSet

import com.tkido.tools.Log
import com.tkido.tools.Text
  
object Parser{
  val readMap = Map(
    ' ' -> FLOOR,
    '.' -> GOAL,
    '@' -> MAN,
    '+' -> (MAN | GOAL),
    '$' -> BAG,
    '*' -> (BAG | GOAL),
    '#' -> WALL)
  
  def apply(path:String) = {
    Log i s"read:${path}"
    val rawLines = Text.readLines(path)
    
    val lines =
      rawLines.filter(line =>
        line.nonEmpty &&
        !line.exists{!readMap.contains(_)}
      )
    Log.d(lines)
    
    val height = lines.size
    val width = lines.map(_.size).max
    val limit = width * height
    Log.d(width, height, limit)
    
    val arr =
      lines.map(_.map(readMap(_)))
        .flatten.toArray
    
    val mans = BitSet()
    val bags = BitSet()
    val goals = BitSet()
    
    arr.zipWithIndex.foreach{case(n, i) =>
       if((n & MAN) == MAN) mans += i
       if((n & BAG) == BAG) bags += i
       if((n & GOAL) == GOAL) goals += i
    }
    
    val naked = arr.map(_ & ~(MAN | BAG))
    Log i mans
    Log i bags
    Log i goals
    (width, arr)
  }
}