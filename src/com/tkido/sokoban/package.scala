package com.tkido

package object sokoban {
  val LARGEINT = 99999999
  
  val FLOOR =  0
  val GOAL  =  1
  val MAN   =  2
  val MONG  =  3   //MAN_ON_GOAL
  val AVOID  = 4
  val MONA  =  6   //MAN_ON_AVOID
  val BAG   =  8
  val BONG  =  9   //BAG_ON_GOAL
  val WALL  = 16
  val EXTRA = 32
  val BLANK = 64
  
  val atoi_map = Map(
    ' ' -> FLOOR,
    '.' -> GOAL,
    '@' -> MAN,
    '+' -> MONG,
    '$' -> BAG,
    '*' -> BONG,
    '#' -> WALL)
  
  val itoa_map = Map(
    FLOOR -> ' ',
    GOAL  -> '.',
    MAN   -> '@',
    MONG  -> '+',
    AVOID -> 'x',
    MONA  -> '@',
    BAG   -> '$',
    BONG  -> '*',
    WALL  -> '#',
    EXTRA -> ' ',
    BLANK -> ' ')

  val itom_map = Map(
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
   
  val ORIGIN = Vector(0, 0)
  val I      = Vector(0, 1)
  
  val LEFT  = Vector(-1,  0)
  val UP    = Vector( 0, -1)
  val RIGHT = Vector( 1,  0)
  val DOWN  = Vector( 0,  1)
    
  val UPPER_LEFT  = Vector(-1, -1)
  val UPPER_RIGHT = Vector( 1, -1)
  val LOWER_LEFT  = Vector(-1,  1)
  val LOWER_RIGHT = Vector( 1,  1)
  
  val NEUMANN = List(UP, LEFT, RIGHT, DOWN)
  val MOORE = List(UPPER_LEFT, UP, UPPER_RIGHT, LEFT, RIGHT, LOWER_LEFT, DOWN, LOWER_RIGHT)
  val STAR = List(UPPER_LEFT, UPPER_RIGHT, ORIGIN, LOWER_LEFT, LOWER_RIGHT)
  
  //for Node.Id, Node.parent
  val SPECIALINITNODE = -1
  //for Node.status
  val UNKNOWN = 0
  val CHECKED = 1
  val LIVE = 3
  val DEAD = 4
  
  /*
  class ClearException extends Exception
  class ImpossibleException extends Exception
  */
}