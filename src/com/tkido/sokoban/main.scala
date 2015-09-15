package com.tkido.sokoban
import scala.collection.mutable.BitSet

object main extends App {
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  val data = Parser(args.head)
  val printer = Printer(data)
  Log.d(printer(data))

  val solver = Solver(data, data.pullCounts)
  
  Log.close()
}
