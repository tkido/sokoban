package com.tkido.sokoban
import scala.collection.mutable.BitSet
import com.tkido.tools.Log

class LockChecker(data:Data) {
  private val width = data.width
  private val canBags = data.canBags
  val r = Map(-width -> -1, -1 -> width, width -> 1, 1 -> -width) //Rotation Map: It rotates vector 90 degrees counterclockwise.
  
  def apply(bags:BitSet, to:Int, direction:Int) :Boolean = {
    val locked = BitSet()
    def check(v:Int, d:Int, depth:Int) :Boolean = {
      val rst =
        if(locked(v)){
          true
        }else if(bags(v)){
          locked += v
          (depth > 0 || check(v+d, d, depth+1)) &&
          ((!canBags(v+r(d)) && !canBags(v-r(d))) ||
           check(v+r(d),  r(d), depth+1) ||
           check(v-r(d), -r(d), depth+1) )
        }else{
          !data.canMans(v)
        }
      if(!rst) locked -= v
      rst
    }
    check(to, direction, 0) && !locked.subsetOf(data.goals)
  }
}


object LockChecker{
  def apply(data:Data) = new LockChecker(data)
}