package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack
import com.tkido.tools.Log

class Data(
    val width:Int,
    val height:Int,
    val limit:Int,
    val initMan:Int,
    var man:Int,
    val bags:BitSet,
    val goals:BitSet,
    val canMans:BitSet,
    val canBags:BitSet,
    val naked:Array[Int],
    val neumann:List[Int],
    val steps:Stack[Int]){
  
  setBlank()
  setExtra()
  
  val pushCounts = countPush()
  setAvoid(pushCounts)
  val pullCounts = countPull()
  setAvoid(pullCounts)
  
  
  val initBags = bags
  val orderedGoals = getOrderedGoals(goals).reverse
  Log d s"orderedGoals: ${orderedGoals}"
  
  def getOrderedGoals(bags:BitSet) :List[Int] = {
    if(bags.isEmpty) return List[Int]()
    
    def canRemove(bag:Int) :Boolean = {
      val checked = BitSet()
      val reachableBags = BitSet()
      def check(v:Int) {
        checked += v
        if(initBags(v)) reachableBags += v 
        for (d <- neumann)
          if (!checked(v+d) &&
              canBags(v+d) &&
              !bags(v+d) &&
              canMans(v+d*2) &&
              !bags(v+d*2) &&
              !isDeadEnd(v+d*2, v+d) )
            check(v+d)
      }
      check(bag)
      reachableBags.nonEmpty
    }
    val removableBags = bags.filter(canRemove)
    removableBags.toList ::: getOrderedGoals(bags &~ removableBags)
  }
  
  
  
  
  class GoalOrderer(data:Data) {
  }
  
  
  object GoalOrderer {
    def apply(data:Data) = new GoalOrderer(data)
  }  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  /**
   * Set WALLs to unreachable FLOORs.
   */
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
  
  /**
   * Set EXTRA to useless FLOORs.
   * EXTRA is the same as WALL other than it's appearance.
   * Now "useless" means dead end widthout GOAL.
   */
  private def setExtra() {
    def check(v:Int){
      if(goals(v))
        return
      if(neumann.map(d => if(canMans(v+d)) 0 else 1).sum == 3){
        canMans -= v
        canBags -= v
        naked(v) = EXTRA
        val d = neumann.collectFirst{case d if canMans(v+d) => d}.get
        if(v == man){
          steps.push(d)
          man += d
          if(bags(v+d))
            bags -= (v+d) += (v+d*2)
        }
        check(v+d)
      }
    }
    canMans.foreach{check(_)}
  }
  
  /**
   * Home is the size of space on that MAN can move without pushing bags
   */
  private def getHomes(man:Int, bags:BitSet) :BitSet = {
    def check(v:Int, done:BitSet) :BitSet = {
      done += v
      for (d <- neumann)
        if (!done(v+d) && canMans(v+d) && !bags(v+d))
          check(v+d,done)
      done
    }
    check(man, BitSet())
  }
  
  /**
   * Home is smaller than 5 inevitably, except for initial situation.
   */
  def isDeadEnd(man:Int, bags:BitSet) :Boolean = {
    val homes = getHomes(man, bags)
    homes.size < 5 && !homes(initMan)
  }
  private def getHomes(man:Int, bag:Int) :BitSet =
    getHomes(man, BitSet(bag))
  def isDeadEnd(man:Int, bag:Int) :Boolean =
    isDeadEnd(man, BitSet(bag))
  
  /**
   * Pull a imaginary BAG from each GOAL with counting distance
   */
  def countPull() :Iterable[Array[Int]] = {
    def getPullCounts(goal:Int): Array[Int] = {
      def check(v:Int, distance:Int, counts:Array[Int]) :Array[Int] = {
        counts(v) = distance
        for (d <- neumann)
          if (canMans(v+d) &&
              canMans(v+d*2) &&
              distance+1 < counts(v+d) &&
              !isDeadEnd(v+d*2, v+d) )
            check(v+d, distance+1, counts)
        counts
      }
      check(goal, 0, Array.fill(limit)(LARGEINT))
    }
    goals.map(getPullCounts)
  }
  
  /**
   * Push a imaginary BAG from each GOAL with counting distance
   */
  private def countPush() :Iterable[Array[Int]] = {
    def getPushCounts(goal:Int): Array[Int] = {
      def check(v:Int, distance:Int, counts:Array[Int]) :Array[Int] = {
        counts(v) = distance
        for (d <- neumann)
          if (canMans(v-d) &&
              canBags(v+d) &&
              distance+1 < counts(v+d))
            check(v+d, distance+1, counts)
        counts
      }
      check(goal, 0, Array.fill(limit)(LARGEINT))
    }
    bags.map(getPushCounts)
  }
  
  /**
   * Set AVOIDs to FLOORs on that BAG cannot be.
   */
  private def setAvoid(counts:Iterable[Array[Int]]){
    val avoids =
      canBags.filter{i =>
        !counts.exists{arr =>
          arr(i) != LARGEINT
        }
      }
    avoids.foreach{naked(_) |= AVOID}
    canBags &~= avoids
  }
  

  
}
