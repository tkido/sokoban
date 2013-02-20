package com.tkido.sokoban

class LockChecker(naked:Board) {
  import scala.collection.mutable.{Set => MSet}
  
  private val goals = naked.goals
  
  def isLocked(bags:Set[Vector], hand:Hand) :Boolean = {
    val lockedbags = MSet[Vector]()
    def sub(v:Vector, d:Vector, depth:Int) :Boolean = {
      var rst = false
      if(lockedbags(v)){
        rst = true
      }else if(bags(v)){
        lockedbags += v
        rst = (depth > 0 || sub(v+d, d, depth+1)) &&
              ((naked.isAvoid(v+d*I) && naked.isAvoid(v-d*I)) ||
               sub(v+d*I,  d*I, depth+1) ||
               sub(v-d*I, -d*I, depth+1) )
      }else{
        rst = naked.isWall(v)
      }
      if(!rst) lockedbags -= v
      rst
    }
    val result = sub(hand.to, hand.direction, 0) && (lockedbags &~ goals).nonEmpty
    //if(result) Logger.log1("Locked status checked!!\n", naked.dressed(v-d, bags))
    result
  }
}


object LockChecker{
  def apply(board:Board) = new LockChecker(board)
}