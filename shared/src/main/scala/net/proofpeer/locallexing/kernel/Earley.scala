package net.proofpeer.locallexing.kernel


object Earley {

  final class ItemEnv[P](val size : Int, val dot : Int, values : Vector[P]) extends Grammar.Environment[P] {
    def inputAt(index : Int) : P = {
      if (index == 0) values(index) 
      else if (index > 0 && index <= dot) values(2*index - 1)
      else throw new RuntimeException("invalid call inputAt(" + index+ ")")
    }
    def outputAt(index : Int) : P = {
      if (index > 0 && index <= dot) values(2 * index) 
      else throw new RuntimeException("invalid call outputAt(" + index + ")")
    }
  }

  final case class Item[P](coreItemId : Int, size : Int, values : Vector[P], indices : Vector[Int]) 
  {

    def param : P = values(0)

    def origin : Int = indices(0)

    def dot : Int = indices.length - 1

    def inAt(index : Int) : P = {
      if (index == 0) values(index) else values(2*index - 1)
    }

    def outAt(index : Int) : P = {
      if (index == 0) values(2 * dot + 1) else values(2 * index)
    }

    def result : P = outAt(0)

    def nextParam : P = inAt(dot + 1)

    def mkNextItem(nextCoreItem : CoreItem[P], k : Int, out : P) : Option[Item[P]] = {
      val intermediateValues = values :+ out
      val env = new ItemEnv(size, dot + 1, intermediateValues)
      nextCoreItem.nextFun(env) match {
        case None => None
        case Some(in) => Some(Item[P](nextCoreItem.id, size, intermediateValues :+ in, indices :+ k))
      }
    }

    def span : (Int, Int) = (indices.head, indices.last)

    def spanOf(index : Int) : (Int, Int) = (indices(index - 1), indices(index))

    /*override def toString : String = {
      val p = "{" + param + "}"
      "Earley.Item[coreItemId="+coreItemId+p+", origin="+origin+"]"
    }*/

  }

  trait CoreItem[P] {

    def id : Int

    def nextSymbol : Option[Grammar.Symbol]

    def ruleIndex : Int

    def dot : Int

    def nonterminal : Grammar.NS

    def nextFun : Grammar.EnvFun[P]

    /* This only makes sense for dot == 0 */
    def mkInitialItem(k : Int, param : P) : Option[Item[P]]
    
    // the following functions all fail if there is no appropriate next symbol

    def predictedCoreItems : Vector[CoreItem[P]]

    def nextCoreItem : CoreItem[P]

  }

  final class Kernel[CHAR, P](val grammar : Grammar[CHAR, P]) {

    import Grammar._

    private def computeCoreItems() : (Map[Int, (Int, Int, Int)], Map[(Int, Int, Int), Int], Map[Int, CoreItem[P]]) = {
      var coreItemIds : Map[Int, (Int, Int, Int)] = Map()
      var coreItemIdsRev : Map[(Int, Int, Int), Int] = Map()
      var coreItems : Map[Int, CoreItem[P]] = Map()
      var nextId = 0
      var n = 0
      for (nonterminal <- grammar.nonterminals) {
        var r = 0
        for (rule <- nonterminal.rules) {
          for (dot <- 0 to rule.rhs.size) {
            coreItemIds = coreItemIds + (nextId -> (n, r, dot))
            coreItemIdsRev = coreItemIdsRev + ((n, r, dot) -> nextId)
            nextId += 1
          }
          r += 1
        }
        n += 1
      }
      for ((coreItemId, (n, r, dot)) <- coreItemIds) {
        val ns = NS(n)
        val nonterminal = grammar.get(ns)
        val rule = nonterminal.rules(r)
        val (nextSymbol, nextFun) =
          if (dot == rule.rhs.size) (None, rule.out)
          else (Some(rule.rhs(dot)._1), rule.rhs(dot)._2)
        var predicted : Vector[Int] = Vector()
        var nextCoreItemId : Int = -1
        nextSymbol match {
          case Some(nextns : NS) =>
            val nextnonterminal = grammar.get(nextns)
            for (nextr <- 0 until nextnonterminal.rules.size) {
              val predictedId = coreItemIdsRev((nextns.index, nextr, 0))
              predicted = predicted :+ predictedId
            }
            nextCoreItemId = coreItemId + 1
          case Some(nextts : TS) =>
            nextCoreItemId = coreItemId + 1
          case None =>
        }
        val coreItem = CI(coreItemId, ns, r, rule.rhs.size, dot, nextSymbol, nextFun, predicted, nextCoreItemId)
        coreItems = coreItems + (coreItemId -> coreItem)
      }
      (coreItemIds, coreItemIdsRev, coreItems)
    }

    private val (coreItemIds, coreItemIdsRev, coreItems) = computeCoreItems()

    private case class CI(id : Int, nonterminal : NS, ruleIndex : Int, size : Int, dot : Int, nextSymbol : Option[Symbol], 
      nextFun : EnvFun[P], predictedCoreItemIds : Vector[Int], nextCoreItemId : Int) extends CoreItem[P] 
    {

      lazy val predictedCoreItems : Vector[CoreItem[P]] = predictedCoreItemIds.map(id => coreItems(id))

      lazy val nextCoreItem : CoreItem[P] = coreItems(nextCoreItemId)

      def mkInitialItem(k : Int, param : P) : Option[Item[P]] = {
        val env = new ItemEnv(size, 0, Vector(param))
        nextFun(env) match {
          case None => None
          case Some(q) => Some(Item(id, size, Vector(param, q), Vector(k)))
        }
      }

    }

    def coreItem(coreItemId : Int) : CoreItem[P] = coreItems(coreItemId)

    def coreItemOf(item : Earley.Item[P]) : CoreItem[P] = coreItem(item.coreItemId)

    def numCoreItems : Int = coreItems.size

  }

}


final class Earley[CHAR, P](kernel : Earley.Kernel[CHAR, P], startNonterminal : Grammar.NS = Grammar.NS(0)) {

  import Earley._
  import Grammar._

  type Bin = Set[Item[P]]

  type Bins = Array[Bin]

  def addItem(bin : Bin, item : Item[P]) : Bin = {
    if (bin.contains(item)) bin else {
      //println("adding item: " + item)
      bin + item
    }
  }
  
  def Init() : Bin = {
    var bin : Bin = Set()
    val numCoreItems = kernel.numCoreItems
    for (coreItemId <- 0 until numCoreItems) {
      val coreItem = kernel.coreItem(coreItemId)
      if (coreItem.dot == 0 && coreItem.nonterminal == startNonterminal) {
        coreItem.mkInitialItem(0, kernel.grammar.startParam) match {
          case Some(item) => bin = addItem(bin, item)
          case None =>
        }
      }
    }
    bin
  }

  def binSize(bins : Bins, k : Int) : String = {
    ", binsize = " + bins(k).size
  }

  def Predict(bins : Bins, k : Int) : Boolean = {
    var bin = bins(k)
    val oldSize = bin.size
    for (item <- bin) {
      val coreItem = kernel.coreItemOf(item)
      coreItem.nextSymbol match {
        case Some(ns : NS) => 
          val param = item.nextParam
          for (predictedCoreItem <- coreItem.predictedCoreItems) {
            predictedCoreItem.mkInitialItem(k, param) match {
              case Some(item) => bin = addItem(bin, item)
              case None =>
            }
          }
        case _ => 
      }
    }
    bins(k) = bin
    bin.size != oldSize
  }

  def Complete(bins : Bins, k : Int) : Boolean = {
    var bin = bins(k)
    val oldSize = bin.size
    for (item <- bin) {
      val coreItem = kernel.coreItemOf(item)
      coreItem.nextSymbol match {
        case None =>
          val nextSymbol = Some(coreItem.nonterminal)
          val param = item.param
          for (srcItem <- bins(item.origin)) {
            val srcCoreItem = kernel.coreItemOf(srcItem)
            if (srcCoreItem.nextSymbol == nextSymbol) {
              if (srcItem.nextParam == param) {
                srcItem.mkNextItem(srcCoreItem.nextCoreItem, k, item.result) match {
                  case None =>
                  case Some(item) => bin = addItem(bin, item) 
                }
              }
            }
          }
        case _ =>
      }
    }
    bins(k) = bin
    bin.size != oldSize
  }

  def Tokens(input : Input[CHAR], bins : Bins, prevTokens : Tokens[P], k : Int) : Tokens[P] = {
    var tokens = prevTokens
    val bin = bins(k)
    val grammar = kernel.grammar
    for (item <- bin) {
      val coreItem = kernel.coreItemOf(item)
      coreItem.nextSymbol match {
        case Some(ts : TS) =>
          val param = item.nextParam
          val x = (ts, param)
          tokens.get(x) match {
            case None =>
              val result = grammar.get(ts).lexer.lex(input, k, param)
              if (!result.isEmpty)
                tokens = tokens + (x -> result)
            case _ =>
          }
        case _ =>
      }
    }
    tokens = grammar.selector.select(input, k, prevTokens, tokens)
    tokens
  }

  def Scan(bins : Bins, tokens : Tokens[P], k : Int) : Boolean = {
    val bin = bins(k)
    val oldSize = bin.size
    for (item <- bin) {
      val coreItem = kernel.coreItemOf(item)
      coreItem.nextSymbol match {
        case Some(ts : TS) =>
          val param = item.nextParam
          val x = (ts, param)
          tokens.get(x) match {
            case None =>
            case Some(result) =>
              for ((len, out) <- result) {
                val nextCoreItem = coreItem.nextCoreItem
                item.mkNextItem(nextCoreItem, k + len, out) match {
                  case None =>
                  case Some(nextItem) => 
                    bins(k + len) = addItem(bins(k + len), nextItem)
                }
              }
          }
        case _ =>
      }
    } 
    oldSize != bins(k).size
  }

  def Pi(bins : Bins, tokens : Tokens[P], k : Int) : Boolean = {
    var changed = false
    var oldChanged = false
    do {
      oldChanged = changed
      changed = false
      if (Predict(bins, k)) changed = true
      if (Complete(bins, k)) changed = true
      if (Scan(bins, tokens, k)) changed = true
    } while (changed)
    oldChanged
  }

  def computeBin(input : Input[CHAR], bins : Bins, k : Int) {
    var tokens : Tokens[P] = Map()
    Pi(bins, tokens, k)
    while (true) {
      tokens = Tokens(input, bins, tokens, k)
      if (!Pi(bins, tokens, k)) return
    }
  }

  def computeResults(bin : Bin) : Set[P] = {
    val grammar = kernel.grammar
    var results : Set[P] = Set()
    for (item <- bin) {
      if (item.origin == 0) {
        val coreItem = kernel.coreItemOf(item)
        if (coreItem.nonterminal == startNonterminal && coreItem.nextSymbol == None 
          && item.param == grammar.startParam)
          results = results + item.result
      }
    }
    results
  }

  def recognize(input : Input[CHAR]) : Either[(Array[Bin], Set[P]), Int] = {
    val inputSize = input.size
    val bins : Bins = new Array(inputSize + 1)
    for (k <- 0 to inputSize) bins(k) = Set()
    bins(0) = Init()
    for (k <- 0 to inputSize) computeBin(input, bins, k)
    val results = computeResults(bins(inputSize))
    if (results.isEmpty) {
      var k = inputSize
      var foundNonemptyBin = false
      while (k >= 0 && !foundNonemptyBin) {
        if (bins(k).size > 0) 
          foundNonemptyBin = true
        else k -= 1
      }
      Right(k)
    } else {
      Left((bins, results))
    } 
  }

  private class ParseTreeConstruction(bins : Array[Bin]) {

    import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
    private val cache : MutableMap[(Grammar.NS, P, P, Int, Int), ParseTree[P]] = MutableMap()
    private val visiting : MutableSet[(Grammar.NS, P, P, Int, Int)] = MutableSet()
    private val cycles : MutableMap[(Grammar.NS, P, P, Int, Int), ParseTree.CycleNode[P]] = MutableMap()

    def fillCycles() {
      for ((key, tree) <- cache) {
        cycles.get(key) match {
          case None => // do nothing
          case Some(cycle) => cycle.tree = tree
        }  
      }
    }

    def getParseTree(nonterminal : Grammar.NS, param : P, result : P,
      startPosition : Int, endPosition : Int) : ParseTree[P] = 
    {
      val key = (nonterminal, param, result, startPosition, endPosition)
      cache.get(key) match {
        case None => 
          if (visiting(key)) {
            cycles.get(key) match {
              case None =>
                val cycle = new ParseTree.CycleNode[P](nonterminal, (startPosition, endPosition), param, result)
                cycles += (key -> cycle)
                return cycle
              case Some(cycle) =>
                return cycle 
            }
          }
          visiting += key
          val r = constructParseTree(nonterminal, param, result, startPosition, endPosition)
          cache += (key -> r)
          visiting -= key
          r
        case Some(r) =>
          r
      }
    }

    /** Constructs the parse tree using the information obtained from the recognition phase. This assumes that there actually exists at least one parse tree.
      * @param startPosition the start position (inclusive)
      * @param endPosition the end position (exclusive)
      */
    def constructParseTree(nonterminal : Grammar.NS, param : P, result : P, 
      startPosition : Int, endPosition : Int) : ParseTree[P] = 
    {
      import ParseTree._
      val grammar = kernel.grammar
      val bin = bins(endPosition)
      var foundItems : List[Item[P]] = List()
      for (item <- bin) {
        val coreItem = kernel.coreItemOf(item)
        if (coreItem.nonterminal == nonterminal && coreItem.nextSymbol == None && 
          item.origin == startPosition && item.param == param && item.result == result) 
        {
          foundItems = item :: foundItems
        }
      }
      def mkTree(foundItem : Item[P]) : NonterminalNode[P] = {
        val coreItem = kernel.coreItemOf(foundItem)
        val rule = grammar.get(coreItem.nonterminal).rules(coreItem.ruleIndex)
        var subtrees = new Array[ParseTree[P]](rule.rhs.size)
        var hasAmbiguities = false
        for (i <- 0 until subtrees.size) {
          val symbol = rule.rhs(i)._1
          val child = i + 1
          val span = foundItem.spanOf(child)
          symbol match {
            case ts : TS => 
              subtrees(i) = TerminalNode[P](ts, span, foundItem.inAt(child), foundItem.outAt(child))
            case ns : NS =>
              subtrees(i) = getParseTree(ns, foundItem.inAt(child), foundItem.outAt(child), span._1, span._2)
              if (subtrees(i) == null) return null
          }
          hasAmbiguities = hasAmbiguities || subtrees(i).hasAmbiguities
        }
        NonterminalNode[P](coreItem.nonterminal, coreItem.ruleIndex, foundItem.span, subtrees.toVector, 
          foundItem.param, foundItem.result)
      }
      foundItems match {
        case List() => throw new RuntimeException("cannot construct parse tree for " + 
          nonterminal + " from " + startPosition + " to " + endPosition)
        case List(foundItem) => mkTree(foundItem)
        case _ =>
          val trees = foundItems.map(mkTree _).toVector
          val node = trees.head
          if (trees.size == 1) node
          else AmbiguousNode[P](node.symbol, node.span, trees, node.input, node.output)
      }
    } 

  }

  def parse(input : Input[CHAR]) : Either[Vector[ParseTree[P]], Int] = {
    recognize(input) match {
      case Left((bins, results)) =>
        val ptc = new ParseTreeConstruction(bins)
        var parsetrees : Vector[ParseTree[P]] = Vector()
        for (result <- results)
          parsetrees = parsetrees :+ ptc.getParseTree(startNonterminal, kernel.grammar.startParam, result, 0, input.size)
        ptc.fillCycles()
        Left(parsetrees)
      case Right(k) => 
        Right(k) 
    }
  } 

}