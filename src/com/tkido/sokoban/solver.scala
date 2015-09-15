package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import com.tkido.tools.Log

class Solver(data:Data) {
  val ider = Identifier(data)
  val lockChecker = LockChecker(data)
  val evaluator = Evaluator(data, data.pullCounts)
  val printer = Printer(data)

  val initId = ider.toId(data.man, data.bags)
  val initNode = Node(initId, None, 0, evaluator(data.bags), false, Node.UNKNOWN)
  Log i printer(initNode)
  
  val goals = data.goals
  val canMans = data.canMans
  val canBags = data.canBags
  val neumann = data.neumann
  
  var total = 0
  val nodes = MMap[BigInt, Node]()
  var stack = Stack[BigInt]()
  val stacks = Stack[Stack[BigInt]]()
  var bin = Stack[BigInt]()
  val bins = Stack[Stack[BigInt]]()
  
  Log f printer(initNode)
  solve(initNode) match{
    case None => Log f "Impossible!!"
    case Some(node) => {
      Log f "Cleared!!"
      Log f printer(node)
    }
  }
  
  private def solve(intNode:Node) :Option[Node] = {
    var node = intNode
    stacks.push(stack)
    bins.push(bin)
    stack = Stack[BigInt]()
    bin = Stack[BigInt]()
    nodes(node.id) = node
    stack.push(node.id)
    val depth = stacks.size
    var count = 0
    
    def loop() :Option[Node] = {
      while(stack.nonEmpty){
        total += 1
        count += 1
        Log i s"(${count}/${total}:${depth})th evaluation"
        var id = stack.pop()
        bin.push(id)
        node = nodes(id)
        evaluate(node) match{
          case None => ()
          case Some(node) => return Some(node)
        }
        stack = stack.sortBy(-nodes(_).value)
      }
      None
    }
    
    loop() match{
      case None => None
      case Some(node) => return Some(node)
    }
  }
  
  private def evaluate(node: Node) :Option[Node] = {
    Log d "Start to evaluate this node."
    Log d printer(node)
    if(node.status != Node.UNKNOWN)
      return None
    node.status = Node.CHECKED
    
    val (man, bags) = ider.fromId(node.id)
    if((bags &~ goals).isEmpty)
      return Some(node)
    def check(v:Int, checked:BitSet, hands:MSet[(Int, Int)]) :(BitSet, MSet[(Int, Int)]) = {
      checked += v
      for (d <- neumann){
        if(bags(v+d) && canBags(v+d*2) && !bags(v+d*2))
          hands += Tuple2(v+d, v+d*2)
        if(!checked(v+d) && canMans(v+d) && !bags(v+d))
          check(v+d, checked, hands)
      }
      (checked, hands)
    }
    val (checked, hands) = check(man, BitSet(), MSet[(Int, Int)]())
    
    if(hands.isEmpty){
      node.status = Node.DEAD
      return None
    }
    /*
    val isOpen = (checked.size + bags.size == canMans.size)
    if(isOpen && node.sub)
      return Some(node)
    */
    Log d hands
    hands.foreach{hand =>
      pushBag(hand._1, hand._2)
    }
    def pushBag(from:Int, to:Int){
      val newBags = bags - from + to
      if (lockChecker(newBags, to, to - from)) return
      //if (overchecker.isOvered(hand.to, new_bags)) return
      val newId = ider.toId(from, newBags)
      val newNode = Node(newId, Some(node.id), node.count+1, evaluator(newBags), false, Node.UNKNOWN)
      if(nodes.contains(newId)){
        Log d s"${newId} is Known. status = ${nodes(newId).status}"
        if(!node.sub){
          if(node.count+1 < nodes(newId).count)
            nodes(newId) = newNode
        }
      }else{
        nodes(newId) = newNode
        stack.push(newId)
      }
    }
    None
  }
}

object Solver {
  def apply(problem:Data) =
    new Solver(problem)
}