package com.tkido.sokoban

object main {
  def main(arg:Array[String]):Unit = {
    try{
      if(arg.size >= 2) Logger.level = arg(1).toInt
      val filename = arg(0)
      
      val board = Parser.parse(filename)
      Validator.validate(board)
      val problem = Problem(board)
      val solver = Solver(problem)
      
    }finally{
      Logger.close()
    }
  }
}