package com.tkido.sokoban
import scala.collection.mutable.BitSet
import com.tkido.tools.Log

class Evaluator(data:Data, counts:Iterable[Array[Int]]) {
  val gradient = getGradient(data)
  
  def getValue(bags:BitSet) :Int =
    bags.toList.map(gradient(_)).sum
  
  private def getGradient(data:Data) :Array[Int] = {
    val powedCounts = counts.map(_.map{
      case LARGEINT => 0
      case c => scala.math.pow(c.toDouble, 1.2).toInt
    })
    val powedCountSum = powedCounts
      .fold(Array.fill(data.limit)(0))((a1, a2) => (a1 zip a2).map(p => p._1 + p._2))
    val gradient = powedCountSum.map{
      case 0 => 0
      case c => LARGEINT / c
    }
    data.canBags.foreach{v =>
      if(gradient(v) == 0 && data.goals(v))
        gradient(v) = LARGEINT*2
    }
    Log d s"Evaluator gradient:${gradient.toList}"
    gradient
  }
}

object Evaluator {
  def apply(data:Data, counts:Iterable[Array[Int]]) =
    new Evaluator(data, counts)
}


/*
class OrderedEvaluator(data:Data, counts:Iterable[Array[Int]], orderedgoals:List[Int]) {
  import scala.collection.mutable.{Map => MMap}
  
  val gradientsmap = getGradientsMap()
  Log i ("OrderedEvaluator gradientsmap:\n", gradientsmap)
  
  private def getGradientsMap() :Map[Int, Array[Int]] = {
    val gradients = counts.map(_.map(c => if(c < LARGEINT) LARGEINT/(c+1) else 0))
    val goals = data.goals.toList.sort(_<_)
    goals.zip(gradients).toMap
  }
  
  def getValue(bags:Set[Int]) = {
    def addValue(bags:Set[Int], goals:List[Int]) :Int = {
      if(bags.isEmpty) 0
      else if(!bags(goals.head)) bags.map(gradientsmap(goals.head)(_)).sum
      else LARGEINT + addValue(bags - goals.head, goals.tail)
    }
    addValue(bags:Set[Int], orderedgoals)
  }
  
}


object OrderedEvaluator {
  def apply(data:Data, counts:Iterable[Array[Int]], orderedgoals:List[Int]) =
    new OrderedEvaluator(data, counts, orderedgoals)
}
*/