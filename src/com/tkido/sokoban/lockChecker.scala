package com.tkido.sokoban
import scala.collection.mutable.BitSet
import com.tkido.tools.Log

class LockChecker(data:Data) {
  private val width = data.width
  //Rotation Map: It rotates vector 90 degrees counterclockwise.
  private val r =
    Map(-width -> -1,
        -1 -> width,
        width -> 1,
        1 -> -width)
  
  def apply(bags:BitSet, to:Int, direction:Int) :Boolean = {
    val locked = BitSet()
    def sub(v:Int, d:Int, depth:Int) :Boolean = {
      val rst =
        if(locked(v)){
          true
        }else if(bags(v)){
          locked += v
          (depth > 0 || sub(v+d, d, depth+1)) &&
          ((!data.canBags(v+r(d)) && !data.canBags(v-r(d))) ||
           sub(v+r(d),  r(d), depth+1) ||
           sub(v-r(d), -r(d), depth+1) )
        }else{
          !data.canMans(v)
        }
      if(!rst) locked -= v
      rst
    }
    sub(to, direction, 0) && !locked.subsetOf(data.goals)
  }
}


object LockChecker{
  def apply(data:Data) = new LockChecker(data)
}