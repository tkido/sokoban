package com.tkido.sokoban

class Evaluator(board:Board, pullcounts:List[Grid]) {
  import scala.math.pow
  
  val gradient = getGradient(board)
  
  def getValue(bags:Set[Vector]) :Int =
    bags.toList.map(gradient(_)).sum
  
  private def getGradient(board:Board) :Grid = {
    val powedpullcounts = pullcounts.map(_.map(c => if(c < LARGEINT) pow(c.toDouble, 1.2).toInt else 0))
    for(g <- powedpullcounts) Logger.log3("Evaluator powedpullcount:\n", g)
    val powedpullcountsum = powedpullcounts.foldLeft[Grid](Grid())(_+_)
    Logger.log3("Evaluator powedpullcountsum:\n", powedpullcountsum)
    val gradient = powedpullcountsum.map(c => if(c == 0) 0 else LARGEINT / c)
    val goals = board.goals
    for ((c, v) <- gradient.zipWithVector)
      if(c == 0 && goals.contains(v)) gradient(v) = LARGEINT*2
    Logger.log3("Evaluator gradient:\n" + gradient)
    gradient
  }
  
}


object Evaluator {
  def apply(board:Board, pullcounts:List[Grid]) =
    new Evaluator(board, pullcounts)
}



class OrderedEvaluator(board:Board, pullcounts:List[Grid], orderedgoals:List[Vector]) {
  import scala.collection.mutable.{Map => MMap}
  
  val gradientsmap = getGradientsMap()
  Logger.log3("OrderedEvaluator gradientsmap:\n", gradientsmap)
  
  private def getGradientsMap() :Map[Vector, Grid] = {
    val gradients = pullcounts.map(_.map(c => if(c < LARGEINT) LARGEINT/(c+1) else 0))
    val goals = board.goals.toList.sort(_<_)
    goals.zip(gradients).toMap
  }
  
  def getValue(bags:Set[Vector]) = {
    def addValue(bags:Set[Vector], goals:List[Vector]) :Int = {
      if(bags.isEmpty) 0
      else if(!bags(goals.head)) bags.map(gradientsmap(goals.head)(_)).sum
      else LARGEINT + addValue(bags - goals.head, goals.tail)
    }
    addValue(bags:Set[Vector], orderedgoals)
  }
  
}


object OrderedEvaluator {
  def apply(board:Board, pullcounts:List[Grid], orderedgoals:List[Vector]) =
    new OrderedEvaluator(board, pullcounts, orderedgoals)
}