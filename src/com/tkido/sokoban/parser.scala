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
  
  def apply(path:String) :ProblemInitialData = {
    Log i s"read: ${path}"
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
    
    val arr = lines
                .map(_.map(readMap(_)))
                .map{line => line ++ List.fill(width - line.size)(FLOOR)}
                .flatten.toArray
    
    val mans = BitSet()
    val bags = BitSet()
    val goals = BitSet()
    
    arr.zipWithIndex.foreach{case(n, i) =>
       if((n & MAN) == MAN) mans += i
       if((n & BAG) == BAG) bags += i
       if((n & GOAL) == GOAL) goals += i
    }
    
    assert(mans.size == 1)
    assert(goals.size > 0)
    assert(bags.size > 0)
    assert(bags.size == goals.size)
    
    val naked = arr.map(_ & ~(MAN | BAG))
    
    val neumann = List(-width, -1, 1, width)
    
    new ProblemInitialData(
      width,
      height,
      limit,
      mans.head,
      bags,
      goals,
      naked,
      neumann)
  }
}