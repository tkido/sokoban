package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val (width, naked) = Parser("data/level/simple/map1")
  Log.d(naked.toList)
  
  
  val pullCounts = PullCounter(naked)
  Log.d(pullCounts)
  
  val printer = Printer(width)
  Log.d(printer(naked))
  
  AvoidSetter(naked, pullCounts)
  Log.d(printer(naked))

  Log.close()
}
