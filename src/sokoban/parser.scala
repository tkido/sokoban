package com.tkido.sokoban

import com.tkido.tools.Log
import com.tkido.tools.Text
  
object Parser{
  def apply(path:String) = {
    Log.i("read", path)
    val rawLines = Text.readLines(path)
    
    val lines =
      rawLines.filter(line =>
        line.nonEmpty && !line.exists{!readMap.contains(_)}
      )
    Log.d(lines)
    
    val height = lines.size
    val width = lines.map(_.size).max
    Log.d(width, height)
    
    val arr = lines.map(line => line.map(readMap(_))).flatten.toArray
    (width, arr)
  }
}