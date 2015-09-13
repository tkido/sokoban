package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val (width, naked) = Parser("data/level/simple/map1")
  Log.d(naked.toList)
  
  
  val pullcounts = PullCounter(naked).pullcounts
  Log.d(pullcounts)
  
  val printer = Printer(width)
  Log.d(printer(naked))
  
  Log.d(BONG)
  Log.d(writeMap(0))
  
  Log.close()
}
