package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val initData = Parser("data/level/simple/map1")
  Log.d(initData.naked.toList)
  
  val pullCounts = PullCounter(initData)
  Log.d(pullCounts)
  
  val printer = Printer(initData.width)
  Log.d(printer(initData.naked))
  
  AvoidSetter(initData.naked, pullCounts)
  Log.d(printer(initData.naked))

  Log.close()
}
