package com.tkido.sokoban
import scala.collection.mutable.BitSet
import com.tkido.tools.Log

class Evaluator(data:Data) {
  val gradient = getGradient(data)
  
  def apply(bags:BitSet) :Int =
    bags.toList.map(gradient(_)).sum
  
  private def getGradient(data:Data) :Array[Int] = {
    val powedCounts = data.pullCounts.map(_.map{
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
    //Log d s"Evaluator gradient:${gradient.toList}"
    gradient
  }
}

object Evaluator {
  def apply(data:Data) =
    new Evaluator(data)
}


class OrderedEvaluator(data:Data) {
  val gradientsMap = getGradientsMap()
  Log i s"OrderedEvaluator gradientsMap: ${gradientsMap}"
  
  private def getGradientsMap() :Map[Int, Array[Int]] = {
    val gradients = data.pullCounts.map(_.map{
      case LARGEINT => 0
      case c => LARGEINT/(c+1)
    })
    (data.goals.toList zip gradients).toMap
  }
  
  def apply(bags:BitSet) = {
    def addValue(bags:BitSet, goals:List[Int]) :Int = {
      if(bags.isEmpty) 0
      else if(!bags(goals.head)) bags.toList.map(gradientsMap(goals.head)(_)).sum
      else LARGEINT + addValue(bags - goals.head, goals.tail)
    }
    addValue(bags, data.orderedGoals)
  }
  
}


object OrderedEvaluator {
  def apply(data:Data) =
    new OrderedEvaluator(data)
}
