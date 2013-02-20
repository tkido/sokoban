package com.tkido.sokoban

object AvoidSetter {
  def setAvoid(board:Board, pullcounts:List[Grid]) {
    def setAvoidByPull(board:Board) {
      val pullables = pullcounts.map(_.map(c => if(c < LARGEINT) 1 else 0))
      val pullablesum = pullables.foldLeft[Grid](Grid())(_+_)
      Logger.log3("AvoidSetter pullablesum:\n", pullablesum)
      for((cell, v) <- board.zipWithVector)
        if (cell < WALL && pullablesum(v) == 0)
          board(v) |= AVOID
    }
    
    def setAvoidByPush(board:Board) {
      def makePushable(bag:Vector): Grid ={
        val pushcount = Grid(LARGEINT)
        def sub(v:Vector, distance:Int){
          pushcount(v) = distance
          for (d <- NEUMANN)
            if (!board.isWall(v-d)  &&
                board.isFlat(v+d)  &&
                distance+1 < pushcount(v+d))
              sub(v+d, distance+1)
        }
        sub(bag, 0)
        pushcount.map(c => if(c < LARGEINT) 1 else 0)
      }
      val pushables = board.bags.map(makePushable)
      val pushablesum = pushables.foldLeft[Grid](Grid())(_+_)
      Logger.log3("AvoidSetter pushablesum:\n", pushablesum)
      for((cell, v) <- board.zipWithVector)
        if (cell < AVOID && pushablesum(v) == 0)
          board(v) |= AVOID
    }
    
    setAvoidByPull(board)
    setAvoidByPush(board)
  }

}