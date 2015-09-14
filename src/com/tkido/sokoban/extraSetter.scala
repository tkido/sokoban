package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object ExtraSetter {
  def apply(data:Data) {
    def check(v:Int){
      if(data.goals(v))
        return
      if(data.neumann.map(d => if(data.canMans(v+d)) 0 else 1).sum == 3){
        data.canMans -= v
        data.canBags -= v
        Log i v
        data.naked(v) = EXTRA
        val d = data.neumann.collectFirst{case d if data.canMans(v+d) => d}.get
        if(v == data.man){
          data.steps.push(d)
          data.man += d
          if(data.bags(v+d)){
            data.bags - (v+d)
            data.bags + (v+d*2)
          }
        }
        check(v+d)
      }
    }
    data.canMans.foreach{check(_)}
  }
}