package com.tkido.sokoban

object BlankSetter {
  import scala.collection.mutable.{Set => MSet}
  
  def setBlank(board:Board) {
    val checked = MSet[Vector]()
    def check(v:Vector):Unit = {
      checked += v
      for (d <- NEUMANN)
        if (board(v+d) < WALL && !checked(v+d))
          check(v+d)
    }
    check(board.man)
    for((cell, v) <- board.zipWithVector)
      if (cell == FLOOR && !checked(v))
        board(v) = BLANK
  }
}