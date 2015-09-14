package com.tkido.sokoban
import scala.collection.mutable.BitSet
import scala.collection.mutable.Stack
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import com.tkido.tools.Log

class Solver(data:Data, pullCounts:Iterable[Array[Int]]) {  
  val identifier = Identifier(data)
  val lockChecker = LockChecker(data)
  val evaluator = Evaluator(data, pullCounts)
  
  var total = 0
  
  val initNode = Node(
      identifier.getId(data.man, data.bags),
      None,
      0,
      evaluator(data.bags),
      Node.UNKNOWN)
  
  solve(initNode,
        MMap[BigInt, Node](),
        Stack[Stack[BigInt]](),
        Stack[Stack[BigInt]]()
       )match{
    case None => Log f "Impossible!!"
    case Some(node) => Log f "Cleared!!"
  }
  
  private def solve(
      node:Node,
      nodes:MMap[BigInt, Node],
      stacks:Stack[Stack[BigInt]],
      bins:Stack[Stack[BigInt]]
    ) :Option[Node] = {
    
    val stack = Stack[BigInt](node.id)
    val bin = Stack[BigInt]()
    stacks.push(stack)
    bins.push(bin)
    val depth = stacks.size
    nodes(node.id) = node
    
    var count = 0
    while(stack.nonEmpty){
      total += 1
      count += 1
      Log w s"(${depth}:${count}/${total})th evaluation"
      var id = stack.pop()
      bin.push(id)
      evaluate(nodes(id))
    }
    
    
    val result = loop()
    def clearedSub(cleared:Node){
      def addAncestors(node:Node) :List[Node] =
        if(node.parent.isEmpty) List[Node](node)
        else node :: addAncestors(nodes(node.parent.get))
      val list = addAncestors(cleared)
      for(node <- list) node.status = Node.LIVE
      for(id <- bin) nodes(id).uncheck()
    }
    if(result)
      if(depth > 1) clearedSub(node)
      else AnswerShower(problem, identifier, nodes).showAnswer(node) 
    else
      if(depth > 1) for(id <- bin) nodes(id).die
    stack = stacks.pop
    bin = bins.pop
    result
    
    None
  }
  
  
  private def evaluate(node: Node) :Boolean = {
    if(node.isKnown) return false
    node.check()
    
    val (man, bags) = identifier.fromId(node.id)
    val board = naked.dressed(man, bags)
    Logger.log3("ID:%d COUNT:%d VALUE:%d\n".format(node.id, node.count, node.value), board)
    val subflag = bags.size < goals.size
    if((bags &~ goals).isEmpty) return true
    
    val checked = MSet[Vector]()
    val hands = MSet[Hand]()
    def check(v:Vector){
      checked += v
      for (d <- NEUMANN){
        //if(board.isBag(v+d) && board.canBag(v+d*2)) hands += Hand(v+d, v+d*2)
        if(board.isBag(v+d)){
          var range = 2
          while(board.canBag(v+d*range)){
            hands += Hand(v+d, v+d*range)
            range += 1
          }
        }
        if(!checked(v+d) && board.canMan(v+d)) check(v+d)
      }
    }
    check(man)
    
    if(hands.size == 0){ node.die; return false }
    val openflag = (checked.size + bags.size == problem.floorsize)
    if(openflag && subflag) return true
    
    if(!openflag && node.parent.isDefined){
      val (lastman, lastbags) = identifier.fromId(node.parent.get)
      val lasthand = Hand((lastbags &~ bags).head, (bags &~ lastbags).head)
      
      def isClosed(v:Vector, d:Vector) :Boolean = {
        val aims = List[Vector](v+d, v+d*I, v-d*I, v+d+d*I, v+d-d*I)
        for(aim <- aims){
          var newbags = SortedSet[Vector]()
          def check(v:Vector){
            checked += v
            for(d <- MOORE if(board.isBag(v+d))) newbags += v+d
            for(d <- NEUMANN if(!checked(v+d) && board.canMan(v+d))) check(v+d)
          }
          if(!checked(aim) && board.canMan(aim)){
            check(aim)
            //if(newbags.size < bags.size && newbags.size <= 8){
            if(newbags.size < bags.size && newbags.size <= 20){
              val new_id = identifier.getId(lasthand.from, newbags)
              val new_node = Node(new_id, None, 0, evaluator.getValue(newbags))
              
              if(!nodes.contains(new_id)){
                if(!solve(new_node)) return true
              }else{
                Logger.log1("%d is Known %d node!!".format(new_id, nodes(new_id).status))
                nodes(new_id).status match{
                  case DEAD    => return true
                  case LIVE    => 
                  case CHECKED => sys.error("MUST NOT HAPPEN!!")
                  case UNKNOWN => if(!solve(new_node)) return true
                }
              }
            }
          }
        }
        false
      }
      if(isClosed(lasthand.to, lasthand.direction)){
        Logger.log3("Closed status checked!!\n", naked.dressed(man, bags))
        node.die
        return false
      }
    }
    
    def push(hand:Hand) :Boolean = {
      val new_bags = bags - hand.from + hand.to
      if (lockchecker.isLocked(new_bags, hand)) return false
      if (overchecker.isOvered(hand.to, new_bags)) return false
      val new_id = identifier.getId(hand.from, new_bags)
      val new_node = Node(new_id, Some(node.id), node.count+1, evaluator.getValue(new_bags))
      if(nodes.contains(new_id)){
        Logger.log1("%d is Known %d node!!".format(new_id, nodes(new_id).status))
        if(!subflag){
          if(node.count+1 < nodes(new_id).count)
            nodes(new_id) = new_node
        }else{
          nodes(new_id).status match{
            case DEAD    => return false
            case LIVE    => return true
            case CHECKED => return false
            case UNKNOWN => {
              stack.push(new_id)
              stack = stack.sortBy(-nodes(_).value)
            }
          }
        }
      }else{
        nodes(new_id) = new_node
        stack.push(new_id)
        stack = stack.sortBy(-nodes(_).value)
      }
      false
    }
    for(hand <- hands) if(push(hand)) return true
    
    false
  }

}


object Solver {
  def apply(problem:Data) = new Solver(problem)
}