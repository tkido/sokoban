package com.tkido.sokoban

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val solver = Solver(args.head)
  
  Log d ((System.currentTimeMillis - executionStart) + "msec")
  
  Log.close()
}
