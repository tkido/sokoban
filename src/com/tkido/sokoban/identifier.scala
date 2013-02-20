package com.tkido.sokoban

class Identifier(board:Board) {
  import scala.collection.immutable.SortedSet
  import scala.collection.mutable.{Map => MMap}
  import scala.collection.mutable.{Set => MSet}
  
  val map = MMap[Vector, Int]()
  val rmap = MMap[BigInt, Vector]()
  
  val naked = board.naked
  val bagnum = board.bags.size
  val initbags = board.bags
  val initman = getHome(board.man, initbags)
  
  val spacenum = (for(v <- board.vectors) yield{ if (board.isFlat(v)) 1 else 0}).sum
  var space = spacenum
  for(v <- board.vectors) {
    if (board.isFlat(v)){
      space -= 1
      map(v) = space
      rmap(BigInt(space)) = v
    }
  }
  
  val pascal = Array.fill[BigInt](bagnum+1, spacenum)(1)
  for(n <- Range(1, bagnum+1))
    for(m <- Range(1, spacenum))
      pascal(n)(m) = pascal(n)(m-1) + pascal(n-1)(m-1)
  Logger.log1(pascal.deep)
  
  def getId(man:Vector, bags:Set[Vector]):BigInt = {
    val home = getHome(man, bags)
    if (!map.contains(home)) return SPECIALINITNODE
    
    var hash = BigInt(0)
    for((bag, i) <- bags.zipWithIndex)
      hash += pascal(bagnum-i)(map(bag))
    hash = hash * spacenum + map(home)
    hash
  }
  private def getHome(man:Vector, bags:Set[Vector]) :Vector = {
    val checked = MSet[Vector]()
    val homes = MSet[Vector](man)
    def check(v:Vector){
      checked += v
      if(naked.canBag(v) && !bags(v)) homes += v
      for (d <- NEUMANN)
        if (!checked(v+d) && naked.canMan(v+d) && !bags(v))
          check(v+d)
    }
    check(man)
    homes.min
  }
  
  def fromId(id:BigInt) :(Vector, Set[Vector]) = {
    if(id == SPECIALINITNODE) return (initman, initbags)
    
    val home = rmap(id % spacenum)
    var bags = SortedSet[Vector]()
    def sub(x:BigInt, n:Int, m:Int){
      if(n == 0 || x == 0) return
      if(x >= pascal(n)(m)){
        bags += rmap(m)
        sub(x - pascal(n)(m), n - 1, m - 1)
      }else{
        sub(x, n, m - 1)
      }
    }
    sub(id / spacenum, bagnum, spacenum-1)
    (home, bags)
  }
  
}


object Identifier{
  def apply(board:Board) = new Identifier(board)
}