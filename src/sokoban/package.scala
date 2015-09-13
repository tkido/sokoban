package com.tkido
package object sokoban {
  val FLOOR =  0
  val GOAL  =  1
  val MAN   =  2
  val AVOID  = 4
  val BAG   =  8
  val WALL  = 16
  val EXTRA = 32
  val BLANK = 64
  
  val MONG = MAN | GOAL
  val MONA = MAN | AVOID
  val BONG = BAG | GOAL

  val writeMap = Map(
    FLOOR -> '　',
    GOAL  -> '・',
    MAN   -> '足',
    MONG  -> '足', //距
    AVOID -> '×',
    MONA  -> '足',
    BAG   -> '田',
    BONG  -> '回',
    WALL  -> '■',
    EXTRA -> '△',
    BLANK -> '　')
}