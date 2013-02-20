package com.tkido.sokoban
import scala.collection.mutable.{Map => MMap}

class AnswerShower(problem:Problem, identifier:Identifier, nodes:MMap[BigInt, Node]) {
  val board = problem.board
  
  val init_man = board.man
  val init_bags = board.bags
  val naked = board.naked
  
  def showAnswer(cleared:Node){
    def addAncestors(node:Node) :List[Node] =
      if(node.parent.isEmpty) List[Node](node)
      else node :: addAncestors(nodes(node.parent.get))
    val list = addAncestors(cleared)
    
    def nodeToHandSize(node:Node) :Int = {
      if(node.parent.isEmpty) return 0
      val (man, bags) = identifier.fromId(node.id)
      val (last, lastbags) = identifier.fromId(node.parent.get)
      val hand = Hand((lastbags &~ bags).head, (bags &~ lastbags).head)
      hand.size
    }
    val pushed = list.map(nodeToHandSize).sum
    Logger.log5("Cleared with %d pushes, %d ansestors!!".format(pushed, list.size-1))
    
    def nodeToString(node:Node) :String = {
      if(node.parent.isEmpty)
        return naked.dressed(init_man, init_bags).toString
      val (man, bags) = identifier.fromId(node.id)
      val (last, lastbags) = identifier.fromId(node.parent.get)
      val virtual_man = (lastbags &~ bags).head
      naked.dressed(virtual_man, bags).toString
    }
    Logger.log5(list.reverse.map(nodeToString).mkString("\n"))
  }
}


object AnswerShower {
  def apply(problem:Problem, identifier:Identifier, nodes:MMap[BigInt, Node]) =
    new AnswerShower(problem, identifier, nodes)
}