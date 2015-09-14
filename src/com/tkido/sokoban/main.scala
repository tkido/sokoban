package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val data = Parser(args.head)
  val printer = Printer(data)
  Log.d(printer(data))

  BlankSetter(data)
  Log.d(printer(data))
  
  ExtraSetter(data)
  Log.d(printer(data))
  
  val pullCounts = PullCounter(data)
  Log.d(pullCounts)
  AvoidSetter(data, pullCounts)
  Log.d(printer(data))
  
  val pushCounts = PushCounter(data)
  AvoidSetter(data, pushCounts)  
  Log.d(printer(data))
  
  val evaluator = Evaluator(data, pullCounts)
  //Log d evaluator.gradient.toList
  
  Log.close()
}
