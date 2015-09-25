package com.tkido.sokoban

object Main extends App {
  import java.io.File
  import com.tkido.sokoban.Config
  import com.tkido.tools.Log
  import com.tkido.tools.Text

  Log.level = Config.logLevel
  
  def listFiles(filter:File => Boolean)(file:File): List[File] =
    if (file.isDirectory)
      file.listFiles.toList.flatMap(listFiles(filter))
    else
      List(file).filter(filter)
  def isValid(file:File) :Boolean =
    true
  val root = new File(args.head)
  val files = listFiles(isValid)(root)
  
  Log d files
  
  files.foreach {file =>
    val now = System.currentTimeMillis
    val solver = Solver(file.toString)
    Log f s"${solver}\t${System.currentTimeMillis - now}"
  }
  
  Log.close()
}
