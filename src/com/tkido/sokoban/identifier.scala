package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.{Map => MMap}
import com.tkido.tools.Log

class Identifier(data:Data) {
  val fMap = MMap[Int, Int]()
  val rMap = MMap[BigInt, Int]()
  
  val bagNum = data.bags.size
  val initBags = data.bags.clone
  val initMan = getHome(data.man, initBags)
  val minHome = data.canBags.min
  
  val spaceNum = data.canBags.sum
  var space = spaceNum
  data.canBags.foreach{v =>
    space -= 1
    fMap(v) = space
    rMap(BigInt(space)) = v
  }
 
  val pascal = Array.fill[BigInt](bagNum+1, spaceNum)(1)
  for(n <- Range(1, bagNum+1))
    for(m <- Range(1, spaceNum))
      pascal(n)(m) = pascal(n)(m-1) + pascal(n-1)(m-1)
  
  def toId(man:Int, bags:BitSet):BigInt = {
    val home = getHome(man, bags)
    if (!fMap.contains(home)) return INITNODE
    
    var hash = BigInt(0)
    for((bag, i) <- bags.zipWithIndex)
      hash += pascal(bagNum-i)(fMap(bag))
    hash = hash * spaceNum + fMap(home)
    hash
  }
  
  
  private def getHome(man:Int, bags:BitSet) :Int = {
    class GlobalExitException extends RuntimeException
    def check(v:Int, checked:BitSet, homes:BitSet) :BitSet = {
      checked += v
      if(data.canBags(v) && !bags(v)){
        if(v == minHome) throw new GlobalExitException
        homes += v
      }
      for (d <- data.neumann)
        if (!checked(v+d) && data.canMans(v+d) && !bags(v))
          check(v+d, checked, homes)
      homes
    }
    try{
      val homes = check(man, BitSet(), BitSet())
      if(homes.isEmpty) data.initMan else homes.min
    }catch{
      case e:GlobalExitException => minHome
    }
  }
  
  def fromId(id:BigInt) :(Int, BitSet) = {
    if(id == INITNODE) return (initMan, initBags)
    
    val home = rMap(id % spaceNum)
    val bags = BitSet()
    def sub(x:BigInt, n:Int, m:Int){
      if(n == 0 || x == 0) return
      if(x >= pascal(n)(m)){
        bags += rMap(m)
        sub(x - pascal(n)(m), n - 1, m - 1)
      }else{
        sub(x, n, m - 1)
      }
    }
    sub(id / spaceNum, bagNum, spaceNum-1)
    (home, bags)
  }
}


object Identifier{
  def apply(data:Data) = new Identifier(data)
}