package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import com.tkido.tools.Log

class Solver(data:Data) {
  val ider = Identifier(data)
  val lockChecker = LockChecker(data)
  val overChecker = OverChecker(data)
  val evaluator = Evaluator(data)
  val printer = Printer(data)

  val initId = ider.toId(data.man, data.bags)
  val initNode = Node(initId, None, 0, evaluator(data.bags), false, Node.UNKNOWN)
  Log i printer(initNode)
  
  val goals = data.goals
  val canMans = data.canMans
  val canBags = data.canBags
  val neumann = data.neumann
  //Rotation Map: It rotates vector 90 degrees counterclockwise.
  val width = data.width
  val r = Map(-width -> -1,
              -1 -> width,
              width -> 1,
              1 -> -width)
  val moore = List(-width-1, -width, -width+1, -1, 1, width-1, width, width+1)
  
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
    
    val op = loop() match{
      case None => None
      case Some(node) =>
        if(depth == 1)
          return Some(node) //Clear!!
        else{
          Log d "SubClear!!"
          def addAncestors(node:Node) :List[Node] =
            if(node.parent.isEmpty) List[Node](node)
            else node :: addAncestors(nodes(node.parent.get))
          val list = addAncestors(node)
          for(node <- list) node.status = Node.LIVE
          for(id <- bin) nodes(id).status = Node.UNKNOWN
          Some(node)
        }
    }
    stack = stacks.pop
    bin = bins.pop
    op
  }
  
  private def evaluate(node: Node) :Option[Node] = {
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
    val isOpen = (checked.size + bags.size == canMans.size)
    if(isOpen && node.sub) return Some(node)
    
    if(!isOpen && node.parent.isDefined){
      val lastBags = ider.fromId(node.parent.get)._2
      val lastHand = Tuple2((lastBags &~ bags).head, (bags &~ lastBags).head)
      Log d s"lastHand: ${lastHand}"
      def isClosed(v:Int, d:Int) :Boolean = {
        Log d s"isClosed: v = ${v}, d = ${d}"
        val aims = List(v+d, v+r(d), v-r(d), v+d+r(d), v+d-r(d))
        Log d s"aims: ${aims}"
        for(aim <- aims){
          val newBags = BitSet()
          def check(v:Int){
            Log d s"check: v = ${v}"
            checked += v
            for(d <- moore if(bags(v+d))){
              Log d s"added: v+d = ${v+d}"
              newBags += v+d
            }
            for(d <- neumann if(!checked(v+d) && canMans(v+d) && !bags(v+d))) check(v+d)
          }
          if(!checked(aim) && canMans(aim) && !bags(aim)){
            check(aim)
            Log d s"aim: ${aim}"
            Log d s"checked: ${checked}"
            Log d s"newBags: ${newBags}"
            if(newBags.size < bags.size){
              val newId = ider.toId(lastHand._1, newBags)
              val newNode = Node(newId, None, 0, evaluator(newBags), true, Node.UNKNOWN)
              
              if(!nodes.contains(newId)){
                solve(newNode) match{
                  case None => return true
                  case Some(node) => () //SubProblem Clear
                }
              }else{
                Log d s"${newId} is Known node!! status = ${nodes(newId).status}"
                nodes(newId).status match{
                  case Node.DEAD    => return true
                  case Node.LIVE    => ()
                  case Node.CHECKED => ()
                  case Node.UNKNOWN => solve(newNode) match{
                                         case None => return true
                                         case Some(node) => () //SubProblem Clear
                                       }
                }
              }
            }
          }
        }
        false
      }
      if(isClosed(lastHand._2, lastHand._2 - lastHand._1)){
        Log w s"Closed status checked!!\n${printer(man, bags)}"
        node.status = Node.DEAD
        None
      }
    }
    
    Log d s"Hands: ${hands}"
    hands.foreach{hand =>
      pushBag(hand._1, hand._2)
    }
    def pushBag(from:Int, to:Int){
      val newBags = bags - from + to
      if (lockChecker(newBags, to, to - from)) return
      if (overChecker(to, newBags)) return
      val newId = ider.toId(from, newBags)
      val newNode = Node(newId, Some(node.id), node.count+1, evaluator(newBags), node.sub, Node.UNKNOWN)
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