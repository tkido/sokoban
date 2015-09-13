package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val (width, arr) = Parser("data/level/simple/map1")
  Log.d(arr.toList)
  
  val printer = Printer(width)
  Log.d(printer(arr))
  
  Log.d(BONG)
  Log.d(writeMap(0))
  
  Log.close()
}
