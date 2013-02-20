package com.tkido.sokoban

class OverChecker(board:Board, pullcounts:List[Grid]) {
  import scala.collection.mutable.{Set => MSet}
  import scala.collection.mutable.{Map => MMap}
  
  val bagssize = board.bags.size
  val naked = board.naked
  val util = Utility(naked)
  
  val pullables = pullcounts.map(_.map(c => if(c < LARGEINT) 1 else 0))
  val pullablesum = pullables.foldLeft[Grid](Grid())(_+_)
  Logger.log3("OverChecker pullablesum:\n", pullablesum)
  
  val limits = MMap[Vector, Int]()
  val cpmap = MMap[Vector, MSet[Vector]]()
  for((c, v) <- pullablesum.zipWithVector){
    if(0 < c && c < bagssize){
      val checkpoints = makeCheckPoints(c, v)
      if(c < checkpoints.size){
        limits(v) = c
        cpmap(v) = checkpoints
        Logger.log3("limit: ", c, "\ncheckpoints:", checkpoints)
      }
    }
  }
  Logger.log3("limits: ", limits, "\ncpmap:", cpmap)
  
  private def makeCheckPoints(limit:Int, point:Vector) :MSet[Vector] = {
    val checked = MSet[Vector]()
    def check(v:Vector){
      checked += v
      for (d <- NEUMANN)
        if (!checked(v+d) &&
            pullablesum(v+d) <= limit &&
            naked.canMan(v-d) &&
            naked.canBag(v+d) &&
            !util.isDeadEnd(v+d, v) )
          check(v+d)
    }
    check(point)
    checked
  }
  
  def isOvered(point:Vector, bags:Set[Vector]) :Boolean = {
    if(limits.contains(point)){
      val result = limits(point) < (cpmap(point) & bags).size
      if(result) Logger.log3("Overed status checked!!\n", naked.dressed(bags))
      result
    }else
      false
  }
}


object OverChecker {
  def apply(naked:Board, pullcounts:List[Grid]) =
    new OverChecker(naked, pullcounts)
}