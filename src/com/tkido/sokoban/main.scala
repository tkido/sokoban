package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val data = Parser(args.head)
  
  val pullCounter = DirectionalPullCounter(data)
  
  val solver = Solver(data)
  
  Log.close()
}
