package com.tkido.sokoban

object Logger {
  import java.io.FileOutputStream
  import java.io.OutputStreamWriter
  
  var level = 0
  
  val start_now = new java.util.Date
  val fileName = "log/" + "%tY_%<tm%<td_%<tH%<tM_%<tS.log".format(start_now)    
  val encode = "UTF-8"
  val append = true
  
  val fileOutPutStream = new FileOutputStream(fileName, append)
  val writer = new OutputStreamWriter( fileOutPutStream, encode )
  
  log5( "***** START at %tY/%<tm/%<td %<tH:%<tM:%<tS *****".format(start_now))
  
  
  def log1(args: Any*) {if (level <= 1) write(args)}
  def log2(args: Any*) {if (level <= 2) write(args)}
  def log3(args: Any*) {if (level <= 3) write(args)}
  def log4(args: Any*) {if (level <= 4) write(args)}
  def log5(args: Any*) {if (level <= 5) write(args)}
  
  def write(args: Seq[Any]){
    writer.write(args.mkString("", "", "\n"))
  }
  
  def close() {
    val end_now = new java.util.Date
    log5( "***** END at %tY/%<tm/%<td %<tH:%<tM:%<tS *****".format(end_now))
    writer.close
  }
  
}