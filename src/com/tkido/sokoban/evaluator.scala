package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.{Map => MMap}
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
  
  def apply(bags:BitSet) :Int = {
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


class DirectionalEvaluator(data:Data) {
  val goals = data.goals
  val neumann = data.neumann
  val canMans = data.canMans
  val canBags = data.canBags
  val limit = data.limit
  val width = data.width
  val initMan = data.initMan  
  
  val gradientsMap = data.directionalPullCounts
    .mapValues(_.mapValues(_.mapValues(c => LARGEINT/(c+1))))
  Log i s"DirectionalEvaluator gradientsMap: ${gradientsMap}"
  
  def apply(man:Int, bags:BitSet) :Int = {
    def addValue(man:Int, bags:BitSet, goals:List[Int]) :Int = {
      if(bags.isEmpty) 0
      else if(!bags(goals.head)){
        def addValue2(bags:BitSet) :Int = {
          Log d bags.size
          if(bags.isEmpty) 0
          else{
            def check(v:Int, checked:BitSet, valueMap:MMap[Int, Int]) :MMap[Int, Int] = {
              checked += v
              for (d <- neumann){
                if(bags(v+d)){
                  Log d s"check v = ${v}, d = ${d}"
                  valueMap(v+d) = 0
                  if(canBags(v+d*2)){
                    if(!valueMap.contains(v+d) || gradientsMap(goals.head)(v+d).contains(-d) || valueMap(v+d) < gradientsMap(goals.head)(v+d).getOrElse(-d, 0)){
                      valueMap(v+d) = gradientsMap(goals.head)(v+d).getOrElse(-d, 0)
                    }
                  }
                }
                if(!checked(v+d) && canMans(v+d) && !bags(v+d))
                  check(v+d, checked, valueMap)
              }
              valueMap
            }
            val valueMap = check(man, BitSet(), MMap())
            //Log d valueMap
            //valueMap.values.sum + addValue2(bags &~ valueMap.keySet)
            valueMap.values.max
          }
        }
        addValue2(bags)
      }
      else LARGEINT + addValue(man, bags - goals.head, goals.tail)
    }
    addValue(man, bags, data.orderedGoals)
  }
}

object DirectionalEvaluator {
  def apply(data:Data) =
    new DirectionalEvaluator(data)
}
