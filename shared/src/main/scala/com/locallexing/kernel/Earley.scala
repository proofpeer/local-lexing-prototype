package com.locallexing.kernel

object Earley {

  final case class SymbolData(val firstIndexIncl : Int, val lastIndexExcl : Int, val input : Domain.V, val output : Domain.V) {
    def span : (Int, Int) = (firstIndexIncl, lastIndexExcl)
  }

  final case class Item(val coreItemId : Int, val data : SymbolData, val children : Vector[SymbolData]) extends Domain.Environment
  {

    def param : Domain.V = data.input

    def origin : Int = data.firstIndexIncl

    def inputAt(index : Int) : Domain.V = {
      if (index == 0) data.input else children(index - 1).input
    }

    def outputAt(index : Int) : Domain.V = {
      children(index - 1).output
    }

    override def toString : String = {
      val p = if (param == Domain.V.NIL) "" else "{" + param + "}"
      "Earley.Item[coreItemId="+coreItemId+p+", origin="+origin+"]"
    }

  }

  def debug(s : String) {
    //println("debug: " + s)
  }


  trait CoreItem {

    def nextSymbol : Option[Grammar.Symbol]

    def dot : Int

    def nonterminal : Grammar.NS

    def mkItem(data : SymbolData, children : Vector[SymbolData]) : Option[Item]

    
    // the following functions all fail if there is no appropriate next symbol

    def predictedCoreItems : Vector[CoreItem]

    def nextCoreItem : CoreItem

    def nextSymbolParam(item : Earley.Item) : Domain.V

    def ruleIndex : Int

  }

  final class Kernel[CHAR](val grammar : Grammar[CHAR]) {

    import Grammar._

    private def computeCoreItems() : (Map[Int, (Int, Int, Int)], Map[(Int, Int, Int), Int], Map[Int, CoreItem]) = {
      var coreItemIds : Map[Int, (Int, Int, Int)] = Map()
      var coreItemIdsRev : Map[(Int, Int, Int), Int] = Map()
      var coreItems : Map[Int, CoreItem] = Map()
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
        val (nextSymbol, nextExpr) =
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
        val coreItem = CI(coreItemId, ns, r, dot, nextSymbol, nextExpr, rule.guard, predicted, nextCoreItemId)
        coreItems = coreItems + (coreItemId -> coreItem)
      }
      (coreItemIds, coreItemIdsRev, coreItems)
    }

    private val (coreItemIds, coreItemIdsRev, coreItems) = computeCoreItems()

    private case class CI(id : Int, nonterminal : NS, ruleIndex : Int, dot : Int, nextSymbol : Option[Symbol], nextExpr : Domain.Expr, guard : Domain.Expr, 
      predictedCoreItemIds : Vector[Int], nextCoreItemId : Int) extends CoreItem 
    {

      lazy val predictedCoreItems : Vector[CoreItem] = predictedCoreItemIds.map(id => coreItems(id))

      lazy val nextCoreItem : CoreItem = coreItems(nextCoreItemId)

      def nextSymbolParam(item : Earley.Item) : Domain.V = Domain.Expr.eval(item, nextExpr)
      
      def mkItem(data : SymbolData, children : Vector[SymbolData]) : Option[Item] = {
        val item = Earley.Item(id, data, children)
        if (children.size == 0) {
          Domain.Expr.eval(item, guard) match {
            case Domain.V.BOOL(allow) =>
              if (!allow) return None 
            case _ => throw new RuntimeException("internal error in mkItem")
          }
        }
        nextSymbol match {
          case None =>
            val output = Domain.Expr.eval(item, nextExpr)
            Some(Earley.Item(id, SymbolData(data.firstIndexIncl, data.lastIndexExcl, data.input, output), children))
          case _ => Some(item)
        }
      }

    }

    def coreItem(coreItemId : Int) : CoreItem = coreItems(coreItemId)

    def coreItemOf(item : Earley.Item) : CoreItem = coreItem(item.coreItemId)

    def numCoreItems : Int = coreItems.size

    val startNonterminal = Grammar.NS(0)

  }

}

final class Earley[CHAR](kernel : Earley.Kernel[CHAR]) {

  import Earley._
  import Grammar._

  type Bin = Set[Item]

  type Bins = Array[Bin]
  
  def Init() : Bin = {
    debug("Init")
    var bin : Bin = Set()
    val numCoreItems = kernel.numCoreItems
    for (coreItemId <- 0 until numCoreItems) {
      val coreItem = kernel.coreItem(coreItemId)
      if (coreItem.dot == 0 && coreItem.nonterminal == kernel.startNonterminal) {
        coreItem.mkItem(SymbolData(0, 0, Domain.V.NIL, null), Vector()) match {
          case Some(item) => bin = bin + item
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
    debug("predict " + k + binSize(bins, k))
    var bin = bins(k)
    val oldSize = bin.size
    for (item <- bin) {
      val coreItem = kernel.coreItemOf(item)
      coreItem.nextSymbol match {
        case Some(ns : NS) => 
          val param = coreItem.nextSymbolParam(item)
          for (predictedCoreItem <- coreItem.predictedCoreItems) {
            predictedCoreItem.mkItem(SymbolData(k, k, param, null), Vector()) match {
              case Some(item) => bin = bin + item
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
    debug("complete " + k + binSize(bins, k))
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
              if (srcCoreItem.nextSymbolParam(srcItem) == param) {
                val nextCoreItem = srcCoreItem.nextCoreItem
                val data = srcItem.data
                nextCoreItem.mkItem(SymbolData(data.firstIndexIncl, k, data.input, data.output), srcItem.children :+ item.data) match {
                  case Some(item) => bin = bin + item
                  case None => throw new RuntimeException("internal error")
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

  def Tokens(input : Input[CHAR], bins : Bins, prevTokens : Tokens, k : Int) : Tokens = {
    var tokens = prevTokens
    val bin = bins(k)
    val grammar = kernel.grammar
    for (item <- bin) {
      val coreItem = kernel.coreItemOf(item)
      coreItem.nextSymbol match {
        case Some(ts : TS) =>
          val param = coreItem.nextSymbolParam(item)
          val x = (ts, param)
          tokens.get(x) match {
            case None =>
              grammar.get(ts).lexer.lex(input, k, param) match {
                case Some(result) =>
                  tokens = tokens + (x -> result)
                case None =>
              }
            case _ =>
          }
        case _ =>
      }
    }
    tokens = grammar.selector.select(input, k, prevTokens, tokens)
    debug("found tokens at " + k + ":" + tokens)
    tokens
  }

  def Scan(bins : Bins, tokens : Tokens, k : Int) : Boolean = {
    debug("scan " + k + binSize(bins, k))
    val bin = bins(k)
    val oldSize = bin.size
    for (item <- bin) {
      val coreItem = kernel.coreItemOf(item)
      coreItem.nextSymbol match {
        case Some(ts : TS) =>
          val param = coreItem.nextSymbolParam(item)
          val x = (ts, param)
          tokens.get(x) match {
            case None =>
            case Some((len, out)) =>
              val nextCoreItem = coreItem.nextCoreItem
              val child = SymbolData(k, k + len, param, out)
              val data = item.data
              val nextItem = nextCoreItem.mkItem(SymbolData(data.firstIndexIncl, k + len, data.input, data.output), item.children :+ child).get
              bins(k + len) = bins(k + len) + nextItem
          }
        case _ =>
      }
    }
    oldSize != bins(k).size
  }

  def Pi(bins : Bins, tokens : Tokens, k : Int) : Boolean = {
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
    var tokens : Tokens = Map()
    Pi(bins, tokens, k)
    while (true) {
      tokens = Tokens(input, bins, tokens, k)
      if (!Pi(bins, tokens, k)) return
    }
  }

  def wasRecognized(bin : Bin) : Boolean = {
    for (item <- bin) {
      if (item.origin == 0) {
        val coreItem = kernel.coreItemOf(item)
        if (coreItem.nonterminal == kernel.startNonterminal && coreItem.nextSymbol == None)
          return true
      }
    }
    false
  }

  def recognize(input : Input[CHAR]) : Either[Array[Bin], Int] = {
    val inputSize = input.size
    val bins : Bins = new Array(inputSize + 1)
    for (k <- 0 to inputSize) bins(k) = Set()
    bins(0) = Init()
    for (k <- 0 to inputSize) computeBin(input, bins, k)
    val recognized = wasRecognized(bins(inputSize))
    if (!recognized) {
      var k = inputSize
      var foundNonemptyBin = false
      while (k >= 0 && !foundNonemptyBin) {
        if (bins(k).size > 0) 
          foundNonemptyBin = true
        else k -= 1
      }
      Right(k)
    } else {
      Left(bins)
    } 
  }

  private class ParseTreeConstruction(bins : Array[Bin]) {

    import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
    private val cache : MutableMap[(Grammar.NS, Domain.V, Domain.V, Int, Int), ParseTree] = MutableMap()
    private val visiting : MutableSet[(Grammar.NS, Domain.V, Domain.V, Int, Int)] = MutableSet()

    def getParseTree(nonterminal : Grammar.NS, param : Domain.V, result : Domain.V,
      startPosition : Int, endPosition : Int) : ParseTree = 
    {
      val key = (nonterminal, param, result, startPosition, endPosition)
      cache.get(key) match {
        case None => 
          if (visiting(key)) return null
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
    def constructParseTree(nonterminal : Grammar.NS, param : Domain.V, result : Domain.V, 
      startPosition : Int, endPosition : Int) : ParseTree = 
    {
      import ParseTree._
      val grammar = kernel.grammar
      val bin = bins(endPosition)
      var foundItems : List[Item] = List()
      for (item <- bin) {
        val coreItem = kernel.coreItemOf(item)
        if (coreItem.nonterminal == nonterminal && coreItem.nextSymbol == None && 
          item.data.input == param && item.data.output == result && item.origin == startPosition) 
        {
          foundItems = item :: foundItems
        }
      }
      def mkTree(foundItem : Item) : NonterminalNode = {
        val coreItem = kernel.coreItemOf(foundItem)
        val rule = grammar.get(coreItem.nonterminal).rules(coreItem.ruleIndex)
        var subtrees = new Array[ParseTree](rule.rhs.size)
        var hasAmbiguities = false
        for (i <- 0 until subtrees.size) {
          val symbol = rule.rhs(i)._1
          val child = foundItem.children(i)
          symbol match {
            case ts : TS => 
              subtrees(i) = TerminalNode(ts, child.span, child.input, child.output)
            case ns : NS =>
              subtrees(i) = getParseTree(ns, child.input, child.output, child.firstIndexIncl, child.lastIndexExcl)
              if (subtrees(i) == null) return null
          }
          hasAmbiguities = hasAmbiguities || subtrees(i).hasAmbiguities
        }
        NonterminalNode(coreItem.nonterminal, coreItem.ruleIndex, foundItem.data.span, subtrees.toVector, 
          foundItem.data.input, foundItem.data.output)
      }
      foundItems match {
        case List() => throw new RuntimeException("cannot construct parse tree for " + 
          nonterminal + " from " + startPosition + " to " + endPosition)
        case List(foundItem) => mkTree(foundItem)
        case _ =>
          val trees = foundItems.map(mkTree _).toVector.filter(t => t != null)
          val node = trees.head
          if (trees.size == 1) node
          else AmbiguousNode(node.symbol, node.span, trees, node.input, node.output)
      }
    } 

  }

  def parse(input : Input[CHAR]) : Either[ParseTree, Int] = {
    recognize(input) match {
      case Left(bins) =>
        val ptc = new ParseTreeConstruction(bins)
        Left(ptc.getParseTree(kernel.startNonterminal, Domain.V.NIL, Domain.V.NIL, 0, input.size))
      case Right(k) => 
        Right(k) 
    }
  }   

}