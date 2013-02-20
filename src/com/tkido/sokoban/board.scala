package com.tkido.sokoban

class Board(w:Int, h:Int) extends Grid(w:Int, h:Int){
  def copy :Board = {
    val copied = Board()
    for (v <- vectors) copied(v) = apply(v)
    copied
  }
  
  def findPointOf(n:Int): Option[Vector] =
    findPointOf(c => (c & n) != 0)
  def findPointsOf(n:Int): Set[Vector] =
    findPointsOf(c => (c & n) != 0)
  
  def man:Vector = findPointOf(MAN).get
  def mans :Set[Vector] = findPointsOf(MAN)
  def bags :Set[Vector] = findPointsOf(BAG)
  def goals :Set[Vector] = findPointsOf(GOAL)
  
  def isMan(v:Vector) :Boolean =
    (apply(v) & MAN) != 0
  def isBag(v:Vector) :Boolean =
    (apply(v) & BAG) != 0
  def isGoal(v:Vector) :Boolean =
    (apply(v) & GOAL) != 0
  def isAvoid(v:Vector) :Boolean =
    (apply(v) & AVOID) != 0
  def isWall(v:Vector) :Boolean =
    apply(v) >= WALL
  def isFlat(v:Vector) :Boolean =
    !isAvoid(v) && !isWall(v)
  def canBag(v:Vector) :Boolean =
    apply(v) < AVOID
  def canMan(v:Vector) :Boolean =
    apply(v) < BAG
  
  def naked :Board = {
    val copied = copy
    val manorbag = MAN | BAG
    for (v <- copied.vectors) copied(v) &= ~manorbag
    copied
  }
  def dressed(bags:Set[Vector]) :Board = {
    val copied = copy
    for (bag <- bags) copied(bag) |= BAG 
    copied
  }
  def dressed(man:Vector, bags:Set[Vector]) :Board = {
    val copied = copy
    copied(man) |= MAN
    for (bag <- bags) copied(bag) |= BAG 
    copied
  }
  
  override def toString = {
    val buf = new StringBuilder
    buf ++= "Ÿ%sŸ\n".format(Range(0, Board.width).foldLeft("")(_+"%2d".format(_)))
    for (j <- Range(0, Board.height)){
      buf ++= "%2d".format(j) 
      for (i <- Range(0, Board.width)){
        buf += itom_map(arrays(j)(i))
      }
      buf ++= "%2d\n".format(j)
    }
    buf ++= "š%sŸ\n".format(Range(0, Board.width).foldLeft("")(_+"%2d".format(_)))
    buf.toString
  }

}
object Board{
  private var width = 0
  private var height = 0
  
  def setDefaultSize(w:Int, h:Int) = {width = w; height = h}
  def apply() = new Board(width, height)

}
