package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val (width, arr) = Parser("data/level/simple/map1")
  Log.d(arr.toList)
  
  Log.close()
}
