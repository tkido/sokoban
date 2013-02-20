package com.tkido.sokoban

object Parser{
  import scala.io.Source

  def parse(path:String) = {
    val s = Source.fromFile(path)
    val rawlines = try s.getLines.toList finally s.close
    
    def isValid(line:String) :Boolean ={
      if(line.size == 0) return false
      try{
        for(char <- line) atoi_map(char)
      }catch{
        case e:NoSuchElementException => return false
      }
      true
    }
    val lines = rawlines.filter(isValid)
    val height = lines.length
    val width = lines.map(_.length).max
    
    Grid.setDefaultSize(width, height)
    Board.setDefaultSize(width, height)
    
    val board = Board()
    for((line, j) <- lines.zipWithIndex)
      for((char, i) <- line.zipWithIndex)
        board(i, j) = atoi_map(char)
    board
  }
}