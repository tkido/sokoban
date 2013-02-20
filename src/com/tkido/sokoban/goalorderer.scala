package com.tkido.sokoban

class GoalOrderer(var board:Board) {
  import scala.collection.mutable.{Set => MSet}
  
  val naked = board.naked
  val util = Utility(naked)
  val initbags = board.bags
  
  val orderedgoals = getOrderedGoals(board.goals).reverse
  Logger.log5("GoalOrderer orderedgoals:", orderedgoals)
  
  def getOrderedGoals(bags:Set[Vector]) :List[Vector] = {
    if(bags.isEmpty) return List[Vector]()
    
    val board = naked.dressed(bags)
    def canRemoveAlone(bag:Vector) :Boolean = {
      val checked = MSet[Vector]()
      val reachablebags = MSet[Vector]()
      def check(v:Vector) {
        checked += v
        if(initbags(v)) reachablebags += v 
        for (d <- NEUMANN)
          if (!checked(v+d) &&
              board.canBag(v+d) &&
              board.canMan(v+d*2) &&
              !util.isDeadEnd(v+d*2, v+d) )
            check(v+d)
      }
      check(bag)
      reachablebags.nonEmpty
    }
    val removablebags = bags.filter(canRemoveAlone)
    removablebags.toList ::: getOrderedGoals(bags &~ removablebags)
  }
  
}


object GoalOrderer {
  def apply(board:Board) = new GoalOrderer(board)
}