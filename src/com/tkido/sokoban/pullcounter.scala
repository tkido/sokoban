package com.tkido.sokoban

class PullCounter(naked:Board) {
  private val util = Utility(naked)
  val pullcounts = makePullCounts(naked)
  
  private def makePullCounts(naked:Board) :List[Grid] = {
    def getPullCount(goal:Vector): Grid = {
      val pullcount = Grid(LARGEINT)
      def check(v:Vector, distance:Int):Unit = {
        pullcount(v) = distance
        for (d <- NEUMANN)
          if (!naked.isWall(v+d)   &&
              !naked.isWall(v+d*2) &&
              distance+1 < pullcount(v+d) &&
              !util.isDeadEnd(v+d*2, v+d) )
            check(v+d, distance+1)
      }
      check(goal, 0)
      Logger.log3("PullCounter pullcount:\n", pullcount)
      pullcount
    }
    naked.goals.toList.sort(_<_).map(getPullCount)
  }
}


object PullCounter {
  def apply(naked:Board) = new PullCounter(naked)
}