package com.tkido.sokoban

import scala.collection.mutable.BitSet
import com.tkido.tools.Log

object AvoidSetter {
  def apply(board:Array[Int], pullCounts:List[Array[Int]]) {
    def setAvoidByPull(board:Array[Int]) {
      val avoids =
        Range(0, board.size).filter{i =>
          !pullCounts.exists{arr =>
            arr(i) != Int.MaxValue
          }
        }.toList
      avoids.foreach{case avoid =>
        if(board(avoid) < WALL)
          board(avoid) |= AVOID
      }
    }
    /*
    def setAvoidByPush(board:Array[Int]) {
      def makePushable(bag:Vector): Grid ={
        val pushcount = Grid(LARGEINT)
        def sub(v:Vector, distance:Int){
          pushcount(v) = distance
          for (d <- NEUMANN)
            if (!board.isWall(v-d)  &&
                board.isFlat(v+d)  &&
                distance+1 < pushcount(v+d))
              sub(v+d, distance+1)
        }
        sub(bag, 0)
        pushcount.map(c => if(c < LARGEINT) 1 else 0)
      }
      val pushables = board.bags.map(makePushable)
      val pushablesum = pushables.foldLeft[Grid](Grid())(_+_)
      Logger.log3("AvoidSetter pushablesum:\n", pushablesum)
      for((cell, v) <- board.zipWithVector)
        if (cell < AVOID && pushablesum(v) == 0)
          board(v) |= AVOID
    }
    */
    setAvoidByPull(board)
    //setAvoidByPush(board)
  }

}