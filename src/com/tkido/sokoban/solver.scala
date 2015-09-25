package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import com.tkido.tools.Log

class Solver(path:String) {
  class MustNotHappenException extends RuntimeException
  
  val data = Parser(path)
  
  val ider = Identifier(data)
  val lockChecker = LockChecker(data)
  val overChecker = OverChecker(data)
  val evaluator = DirectionalEvaluator(data) //OrderedEvaluator(data) //Evaluator(data)
  val printer = Printer(data)
  
  val Hand = HandFactory(data.width)

  val initId = ider.toId(data.man, data.bags)
  val initNode = Node(initId, None, 0, evaluator(data.man, data.bags, None), false, null, Node.UNKNOWN)
  Log i printer(initNode)
  
  val goals = data.goals
  val canMans = data.canMans
  val canBags = data.canBags
  val neumann = data.neumann
  val width = data.width
  
  val r = Map(-width -> -1, -1 -> width, width -> 1, 1 -> -width) //Rotation Map: It rotates vector 90 degrees counterclockwise.
  val moore = List(-width-1, -width, -width+1, -1, 1, width-1, width, width+1) //Moore neighborhood. See for details "https://en.wikipedia.org/wiki/Moore_neighborhood".
  
  var total = 0
  val nodes = MMap[BigInt, Node]()
  val todos = Stack[Stack[BigInt]]()
  val dones = Stack[Stack[BigInt]]()
  var todo = Stack[BigInt]()
  var done = Stack[BigInt]()
  var node = initNode
  
  Log w printer(initNode)
  
  val isCleared = solve(initNode)
  
  override def toString = {
    s"${path}\t${data.width}\t${data.height}\t${data.bags.size}\t${isCleared}\t${node.count}\t${total}\t${nodes.size}"
  }
  
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
        Log i s"${depth}(${count}/${total})th evaluation(todo.size = ${todo.size})"
        if(total > 5000) return false
        var id = todo.pop()
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
      for(node <- list) node.status = Node.LIVE
      //for(id <- done) Log d s"There are LIVE!!\n${printer(id)}"
      for(id <- done)
        if(nodes(id).status == Node.CHECKED){
          nodes(id).status = Node.UNKNOWN
          //Log d s"There are UNKNOWN!!\n${printer(id)}"
        }
      if(depth == 1) list.reverse.foreach(node => Log f s"${printer(node)}")
    }else{
      for(id <- done) nodes(id).status = Node.DEAD
      //for(id <- done) Log d s"There are DEAD!!\n${printer(id)}"
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
    def check(v:Int, checked:BitSet, reachedBags:BitSet, hands:MSet[Hand]) :(BitSet, BitSet, MSet[Hand]) = {
      checked += v
      for (d <- neumann){
        if(bags(v+d)){
          reachedBags += v+d
          var range = 2
          while(canBags(v+d*range) && !bags(v+d*range)){
            hands += Hand(v+d, v+d*range)
            range += 1
          }
        }
        if(!checked(v+d) && canMans(v+d) && !bags(v+d))
          check(v+d, checked, reachedBags, hands)
      }
      (checked, reachedBags, hands)
    }
    val (checked, reachedBags, hands) = check(man, BitSet(), BitSet(), MSet[Hand]())
    
    if(hands.isEmpty){
      node.status = Node.DEAD
      return false
    }
    val isOpen = (checked.size + bags.size == canMans.size)
    if(isOpen && node.sub) return true
    
    if(!isOpen && node.parent.isDefined){
      def isClosed(hand:Hand) :Boolean = {
        val (v, d) = (hand.to, hand.direction)
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
              val newId = ider.toId(hand.from, newBags)
              val newNode = Node(newId, None, 0, evaluator(hand.from, newBags, None), true, None, Node.UNKNOWN)
              
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
      if(isClosed(node.lastHand.get)){
        Log w s"Closed status checked!!\n${printer(man, bags)}"
        node.status = Node.DEAD
        return false
      }
    }

    def pushBag(hand:Hand) :Boolean = {
      //Log d hand
      val newBags = bags - hand.from + hand.to
      if (lockChecker(newBags, hand.to, hand.direction)) return false
      if (overChecker(hand.to, newBags)) return false
      val newId = ider.toId(hand.from, newBags)
      val newNode = Node(newId, Some(node.id), node.count + hand.size, evaluator(hand.from, newBags, Some(hand)), node.sub, Some(hand), Node.UNKNOWN)
      if(nodes.contains(newId)){
        //Log d s"${newId} is Known. status = ${nodes(newId).status}"
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
    for(hand <- hands) if(pushBag(hand)) return true
    false
  }
  
}

object Solver {
  def apply(path:String) =
    new Solver(path)
}