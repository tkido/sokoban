package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack
import com.tkido.tools.Log

case class Data(
    width:Int,
    height:Int,
    limit:Int,
    var man:Int,
    bags:BitSet,
    goals:BitSet,
    canMans:BitSet,
    canBags:BitSet,
    naked:Array[Int],
    neumann:List[Int],
    steps:Stack[Int]){
  
  setBlank
  setExtra
  
  private def setBlank() {
    def check(v:Int, checked:BitSet) :BitSet = {
      checked += v
      for (d <- neumann)
        if (canMans(v+d) && !checked(v+d))
          check(v+d, checked)
      checked
    }
    val checked = check(man, BitSet())
    (canMans &~ checked).foreach{ naked(_) = WALL }
    canMans &= checked
    canBags &= checked
  }
  
  private def setExtra() {
    def check(v:Int){
      if(goals(v))
        return
      if(neumann.map(d => if(canMans(v+d)) 0 else 1).sum == 3){
        canMans -= v
        canBags -= v
        Log i v
        naked(v) = EXTRA
        val d = neumann.collectFirst{case d if canMans(v+d) => d}.get
        if(v == man){
          steps.push(d)
          man += d
          if(bags(v+d)){
            bags - (v+d)
            bags + (v+d*2)
          }
        }
        check(v+d)
      }
    }
    canMans.foreach{check(_)}
  }
  
}

case class Node(
    id:BigInt,
    parent:Option[BigInt],
    count:Int,
    value:Int,
    sub:Boolean,
    var status:Int){
  
}
object Node{
  val UNKNOWN = 0
  val CHECKED = 1
  val LIVE = 3
  val DEAD = 4
}