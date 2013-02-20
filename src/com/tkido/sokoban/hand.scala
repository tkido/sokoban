package com.tkido.sokoban

case class Hand(from:Vector, to:Vector){
  def delta = to - from
  def direction = delta.regulated
  def size = delta.size
}
