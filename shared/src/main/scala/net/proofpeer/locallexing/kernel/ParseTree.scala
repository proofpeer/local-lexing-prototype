package net.proofpeer.locallexing.kernel

sealed trait ParseTree[P] {
  def symbol : Grammar.Symbol
  def span : (Int, Int)
  def input : P
  def output : P
  def hasAmbiguities : Boolean
  def ambiguities : List[ParseTree.AmbiguousNode[P]]
  def countTrees : Int
}

final object ParseTree {

  final case class AmbiguousNode[P](symbol : Grammar.NS, span : (Int, Int), alternatives : Vector[NonterminalNode[P]], input : P, output : P) extends ParseTree[P]
  {
    def hasAmbiguities = true
    def ambiguities = List(this)
    lazy val countTrees : Int = {
      var c : Int = 0
      for (alternative <- alternatives) c += alternative.countTrees
      c
    }
  }

  final case class NonterminalNode[P](symbol : Grammar.NS, ruleindex : Int, span : (Int, Int), rhs : Vector[ParseTree[P]], input : P, output : P) extends ParseTree[P] {
    lazy val hasAmbiguities = rhs.exists(_.hasAmbiguities)
    def ambiguities = rhs.toList.flatMap(_.ambiguities)
    lazy val countTrees : Int = {
      var c : Int = 1
      for (tree <- rhs) c *= tree.countTrees
      c
    }
  }

  final case class TerminalNode[P](symbol : Grammar.TS, span : (Int, Int), input : P, output : P) extends ParseTree[P] {
    def hasAmbiguities = false
    def ambiguities = List()
    def countTrees : Int = 1
  }

  def label[CHAR, P](grammar : Grammar[CHAR, P], tree : ParseTree[P]) : String = {
    def isNil(x : Any) : Boolean = {
      x.isInstanceOf[Unit]
    }
    val name = grammar.nameOf(tree.symbol)
    if (isNil(tree.input) && isNil(tree.output))
      name
    else
      name + "("+tree.input+","+tree.output+")"
  }

  type Path[P] = Vector[TerminalNode[P]]
  type Paths[P] = Set[Path[P]]
  type Trees[P] = Vector[ParseTree[P]]

  private def combinePaths[P](A : Paths[P], B : Paths[P]) : Paths[P] = {
    var result : Paths[P] = Set()
    for (a <- A) {
      for (b <- B) {
        result = result + (a ++ b)
      }
    }
    result
  }

  def collectPaths[P](tree : ParseTree[P]) : Paths[P] = {
    tree match {
      case node : AmbiguousNode[P] => collectPaths(node.alternatives : _*)
      case node : TerminalNode[P] => Set(Vector(node))
      case node : NonterminalNode[P] =>
        var paths : Paths[P] = Set(Vector())
        for (child <- node.rhs) {
          val childPaths = collectPaths(child)
          paths = combinePaths(paths, childPaths)
        }
        paths
    }
  }

  def collectPaths[P](trees : ParseTree[P]*) : Paths[P] = {
    var paths : Paths[P] = Set()
    for (tree <- trees) {
      paths = paths ++ collectPaths(tree)
    }
    paths
  }

  def countTrees[P](trees : ParseTree[P]*) : Int = {
    var count = 0
    for (tree <- trees) count += tree.countTrees
    count
  }

  private def appendTrees[P](A : Vector[Vector[ParseTree[P]]], B : Trees[P]) : Vector[Vector[ParseTree[P]]] = {
    var result : Vector[Vector[ParseTree[P]]] = Vector()
    for (a <- A) {
      for (b <- B) {
        result = result :+ (a :+ b)
      }
    }
    result
  }  

  def collectTrees[P](tree : ParseTree[P]) : Trees[P] = {
    tree match {
      case node : AmbiguousNode[P] =>
        var trees : Trees[P] = Vector()
        for (alternative <- node.alternatives) {
          trees = trees ++ collectTrees(alternative)
        }
        trees
      case node : TerminalNode[P] => 
        Vector(node)
      case node : NonterminalNode[P] =>
        var trees : Vector[Vector[ParseTree[P]]] = Vector(Vector())
        for (child <- node.rhs) {
          val childTrees = collectTrees(child)
          trees = appendTrees(trees, childTrees)
        }
        trees.map (rhs => NonterminalNode(node.symbol, node.ruleindex, node.span, rhs, node.input, node.output))
    }
  }

  def printPath[CHAR, P](grammar : Grammar[CHAR, P], input : Input[CHAR], path : Path[P]) : String = {
    def printTerminalNode(node : TerminalNode[P]
      ) : String = {
      val id = label(grammar, node)
      val text = input.print(node.span._1, node.span._2 - node.span._1)
      id + "[\"" + text + "\"]"
    }
    if (path.size == 0) return "ε" 
    var s = printTerminalNode(path(0))
    for (i <- 1 until path.size) s = s + " " + printTerminalNode(path(i))
    s
  }

}