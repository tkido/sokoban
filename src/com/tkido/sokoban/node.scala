package com.tkido.sokoban

class Node(val id:BigInt, val parent:Option[BigInt], val count:Int, val value:Int){
  var status = 0
  
  def isKnown() :Boolean = {
    status != UNKNOWN
  }
  
  def live(){
    status = LIVE
  }
  def die(){
    status = DEAD
  }
  def check(){
    status = CHECKED
  }
  def uncheck(){
    if(status == CHECKED) status = UNKNOWN
  }
  
  override def toString = {
    "Node %d: count %d:\n".format(id, count)
  }
}

object Node{
  def apply(id:BigInt, parent:Option[BigInt], count:Int, value:Int) =
    new Node(id, parent, count, value)
}