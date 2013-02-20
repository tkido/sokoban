package com.tkido.sokoban

object ExtraSetter {
  import scala.collection.mutable.{Set => MSet}
  
  def setExtra(problem:Problem) {
    val board = problem.board
    def check(v:Vector){
      if(!board.isWall(v) && !board.isGoal(v)){
        if((for (d <- NEUMANN) yield if (board.isWall(v+d)) 1 else 0).sum == 3) {
          if(board.isMan(v))
            for (d <- NEUMANN)
              if(!board.isWall(v+d))
                problem.move(v, d)
          board(v) = EXTRA
          for (d <- NEUMANN) check(v+d)
        }
      }
    }
    for(v <- board.vectors) check(v)
  }
}