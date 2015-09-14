package com.tkido.sokoban
import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object AvoidSetter {
  def apply(data:ProblemData, counts:Iterable[Array[Int]]){
    val avoids =
      data.canBags.filter{i =>
        !counts.exists{arr =>
          arr(i) != Int.MaxValue
        }
      }
    Log i data.canBags
    avoids.foreach(data.naked(_) |= AVOID)
    data.canBags &~= avoids
    Log i data.canBags
  }
}