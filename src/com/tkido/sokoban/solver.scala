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
  val evaluator = OrderedEvaluator(data) //Evaluator(data)
  val printer = Printer(data)

  val initId = ider.toId(data.man, data.bags)
  val initNode = Node(initId, None, 0, evaluator(data.bags), false, Node.UNKNOWN)
  Log i printer(initNode)
  
  val goals = data.goals
  val canMans = data.canMans
  val canBags = data.canBags
  val neumann = data.neumann
  val width = data.width
  //Rotation Map: It rotates vector 90 degrees counterclockwise.
  val r = Map(-width -> -1, -1 -> width, width -> 1, 1 -> -width)
  //Moore neighborhood. See for details "https://en.wikipedia.org/wiki/Moore_neighborhood".
  val moore = List(-width-1, -width, -width+1, -1, 1, width-1, width, width+1)
  
  var total = 0
  val nodes = MMap[BigInt, Node]()
  val todos = Stack[Stack[BigInt]]()
  val dones = Stack[Stack[BigInt]]()
  var todo = Stack[BigInt]()
  var done = Stack[BigInt]()
  var node = initNode
  
  Log f printer(initNode)
  
  if(solve(initNode))
    Log f s"Clear!!"
  else
    Log f "Impossible!!"
  
  private def solve(initNode:Node) :Boolean = {
    node = initNode
    todos.push(todo)
    dones.push(done)
    todo = Stack[BigInt]()
    done = Stack[BigInt]()
    nodes(node.id) = node
    todo.push(node.id)
    val depth = todos.size
    var count = 0
    
    def loop() :Boolean = {
      while(todo.nonEmpty){
        total += 1
        count += 1
        Log i s"(${count}/${total}:${depth})th evaluation"
        var id = todo.pop()
        Log d s"todo.size = ${todo.size}"
        done.push(id)
        node = nodes(id)
        val result = evaluate(node)
        todo = todo.sortBy(-nodes(_).value)
        if(result) return true
      }
      false
    }
    val result = loop()
    if(result){
      def addAncestors(node:Node) :List[Node] =
        if(node.parent.isEmpty) List[Node](node)
        else node :: addAncestors(nodes(node.parent.get))
      val list = addAncestors(node)
      if(depth == 1) list.reverse.foreach(node => Log f s"${printer(node)}")
      for(node <- list) node.status = Node.LIVE
      for(id <- done)
        if(nodes(id).status == Node.CHECKED)
          nodes(id).status = Node.UNKNOWN
    }else{
      for(id <- done) nodes(id).status = Node.DEAD
    }
    todo = todos.pop
    done = dones.pop
    result
  }
  
  private def evaluate(node:Node) :Boolean = {
    Log d printer(node)
    if(node.status != Node.UNKNOWN) return false
    node.status = Node.CHECKED
    
    val (man, bags) = ider.fromId(node.id)
    if(bags.subsetOf(goals)) return true
    def check(v:Int, checked:BitSet, reachedBags:BitSet, hands:MSet[(Int, Int)]) :(BitSet, BitSet, MSet[(Int, Int)]) = {
      checked += v
      for (d <- neumann){
        if(bags(v+d)){
          reachedBags += v+d
          if(canBags(v+d*2) && !bags(v+d*2))
            hands += Tuple2(v+d, v+d*2) 
        }
        if(!checked(v+d) && canMans(v+d) && !bags(v+d))
          check(v+d, checked, reachedBags, hands)
      }
      (checked, reachedBags, hands)
    }
    val (checked, reachedBags, hands) = check(man, BitSet(), BitSet(), MSet[(Int, Int)]())
    
    if(hands.isEmpty){
      node.status = Node.DEAD
      return false
    }
    val isOpen = (checked.size + bags.size == canMans.size)
    if(isOpen && node.sub) return true
    
    if(!isOpen && node.parent.isDefined){
      val lastBags = ider.fromId(node.parent.get)._2
      val lastHand = Tuple2((lastBags &~ bags).head, (bags &~ lastBags).head)
      def isClosed(v:Int, d:Int) :Boolean = {
        val aims = List(v+d, v+r(d), v-r(d), v+d+r(d), v+d-r(d))
        for(aim <- aims){
          val newBags = BitSet()
          def check(v:Int){
            checked += v
            for(d <- moore if(bags(v+d))) newBags += v+d
            for(d <- neumann if(!checked(v+d) && canMans(v+d) && !reachedBags(v+d))) check(v+d)
          }
          if(!checked(aim) && canMans(aim) && !bags(aim)){
            check(aim)
            if(newBags.size < bags.size){
              val newId = ider.toId(lastHand._1, newBags)
              val newNode = Node(newId, None, 0, evaluator(newBags), true, Node.UNKNOWN)
              
              if(!nodes.contains(newId)){
                if(!solve(newNode)) return true
              }else{
                Log d s"${newId} is Known node!! status = ${nodes(newId).status}"
                nodes(newId).status match{
                  case Node.DEAD    => return true
                  case Node.LIVE    => ()
                  case Node.CHECKED => throw new MustNotHappenException
                  case Node.UNKNOWN => if(!solve(newNode)) return true
 
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
        return false
      }
    }

    def pushBag(from:Int, to:Int) :Boolean = {
      val newBags = bags - from + to
      if (lockChecker(newBags, to, to - from)) return false
      if (overChecker(to, newBags)) return false
      val newId = ider.toId(from, newBags)
      val newNode = Node(newId, Some(node.id), node.count+1, evaluator(newBags), node.sub, Node.UNKNOWN)
      if(nodes.contains(newId)){
        Log d s"${newId} is Known. status = ${nodes(newId).status}"
        if(!node.sub){
          if(node.count+1 < nodes(newId).count)
            nodes(newId) = newNode
        }else{
          nodes(newId).status match{
            case Node.DEAD    => return false
            case Node.LIVE    => return true
            case Node.CHECKED => return false
            case Node.UNKNOWN => todo.push(newId)
          }
        }
      }else{
        nodes(newId) = newNode
        todo.push(newId)
      }
      false
    }
    Log d s"Hands: ${hands}"
    for(hand <- hands) if(pushBag(hand._1, hand._2)) return true
    false
  }
}

object Solver {
  def apply(problem:Data) =
    new Solver(problem)
}