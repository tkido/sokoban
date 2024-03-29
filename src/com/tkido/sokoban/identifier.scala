package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.{Map => MMap}
import com.tkido.tools.Log

class Identifier(data:Data) {
  private val IRREGULAR_INIT_NODE = -1 //Man is out of canBags. It must be initial node 
  
  private val fMap = MMap[Int, Int]()
  private val rMap = MMap[BigInt, Int]()
  
  private val bagNum = data.bags.size
  private val initBags = data.bags.clone
  private val initMan = getHome(data.man, initBags)
  private val minHome = data.canBags.min
  
  private val spaceNum = data.canBags.size
  private var space = spaceNum
  data.canBags.foreach{v =>
    space -= 1
    fMap(v) = space
    rMap(BigInt(space)) = v
  }
 
  private val pascal = Array.fill[BigInt](bagNum+1, spaceNum)(1)
  for(n <- Range(1, bagNum+1))
    for(m <- Range(1, spaceNum))
      pascal(n)(m) = pascal(n)(m-1) + pascal(n-1)(m-1)
  Log d pascal.deep
  def toId(man:Int, bags:BitSet):BigInt = {
    val home = getHome(man, bags)
    if (!fMap.contains(home)) return IRREGULAR_INIT_NODE
    
    var hash = BigInt(0)
    for((bag, i) <- bags.zipWithIndex)
      hash += pascal(bagNum-i)(fMap(bag))
    hash * spaceNum + fMap(home)
  }
  
  private def getHome(man:Int, bags:BitSet) :Int = {
    class FoundException extends RuntimeException
    def check(v:Int, checked:BitSet, homes:BitSet) :BitSet = {
      checked += v
      if(data.canBags(v) && !bags(v)){
        if(v == minHome) throw new FoundException
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
      case e:FoundException => minHome
    }
  }
  
  def fromId(id:BigInt) :(Int, BitSet) = {
    if(id == IRREGULAR_INIT_NODE) return (initMan, initBags)
    
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