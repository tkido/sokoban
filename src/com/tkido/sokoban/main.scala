package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val data = Parser(args.head)
  val printer = Printer(data.width)
  Log.d(printer(data.naked))
  BlankSetter(data)
  
  val pullCounts = PullCounter(data)
  Log.d(pullCounts)
  
  Log.d(printer(data.naked))
  
  AvoidSetter(data.naked, pullCounts)
  Log.d(printer(data.naked))

  Log.close()
}
