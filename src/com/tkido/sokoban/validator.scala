package com.tkido.sokoban

object Validator {  
  def validate(board:Board){
    
    val mans = board.mans
    val bags = board.bags
    val goals = board.goals
    
    assert(mans.size == 1 , "mans.size need to be 1.")
    assert(bags.size == goals.size , "Invalid stage! bags.size != goals.size")
    
    Logger.log5("board:\n", board)
    Logger.log3("bags:", bags)
    Logger.log3("goals:", goals)
    Logger.log3("mans:", mans)
  }
}