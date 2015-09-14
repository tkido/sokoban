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
  
  val ider = Identifier(data)
  val id = ider.getId(data.man, data.bags)
  Log d s"ID: ${id}"
  
  val (man, bags) = ider.fromId(id)
  Log d s"man: ${man}, bags: ${bags}"
  
  data.man = man
  data.bags.clear
  data.bags |= bags
  Log d printer(data)
  
  val lockChecker = LockChecker(data)
  Log d lockChecker(BitSet(12, 21), 21, -data.width)
  
  Log.close()
}
