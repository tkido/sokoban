package com.tkido.sokoban

class Problem(val board:Board) {
  import scala.collection.mutable.{Set => MSet}
  
  val rawboard = board.copy
  var answer = ""
  
  BlankSetter.setBlank(board)
  ExtraSetter.setExtra(this)
  val naked = board.naked
  val pullcounts = PullCounter(naked).pullcounts
  
  AvoidSetter.setAvoid(board, pullcounts)
  
  val goalorderer = GoalOrderer(board)
  
  val floorsize = getFloorSize
  Logger.log4("floorsize:", floorsize)
  
  private def getFloorSize() :Int = {
    val checked = MSet[Vector]()
    def check(v:Vector):Unit = {
      checked += v
      for (d <- NEUMANN)
        if (naked.canMan(v+d) && !checked(v+d))
          check(v+d)
    }
    check(board.man)
    checked.size
  }
  
  def move(v:Vector, d:Vector){
    var char = "" 
    if(board.isMan(v)){
      board(v) &= ~MAN
      board(v+d) |= MAN
      char = Problem.charmap(d)
      if(board.isBag(v+d)) {
        board(v+d) &= ~BAG
        board(v+d*2) |= BAG
        char = char.toUpperCase()
      }
    }
    answer += char
  }
  
}

object Problem {
  def apply(board:Board) = new Problem(board:Board)
  
  val charmap = Map(
    UP    -> "u",
    LEFT  -> "l",
    RIGHT -> "r",
    DOWN  -> "d")
}