package com.tkido.sokoban
import scala.collection.mutable.BitSet

object BlankSetter {
  def apply(data:ProblemInitialData) {
    def check(v:Int, done:BitSet) :BitSet = {
      done += v
      for (d <- data.neumann)
        if (data.canMans(v+d) && !done(v+d))
          check(v+d, done)
      done
    }
    val checked = check(data.man, BitSet())
    
    (data.canMans &~ checked).foreach{
      data.naked(_) = WALL
    }
    
    data.canMans &= checked
    data.canBags &= checked
  }
}