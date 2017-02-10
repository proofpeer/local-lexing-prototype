package com.locallexing.kernel

sealed trait ParseTree {
  def symbol : Grammar.Symbol
  def span : (Int, Int)
  def input : Domain.V
  def output : Domain.V
  def hasAmbiguities : Boolean
  def ambiguities : List[ParseTree.AmbiguousNode]
  def countTrees : Int
}

final object ParseTree {

  final case class AmbiguousNode(symbol : Grammar.NS, span : (Int, Int), alternatives : Vector[NonterminalNode], input : Domain.V, output : Domain.V) extends ParseTree 
  {
    def hasAmbiguities = true
    def ambiguities = List(this)
    lazy val countTrees : Int = {
      var c : Int = 0
      for (alternative <- alternatives) c += alternative.countTrees
      c
    }
  }

  final case class NonterminalNode(symbol : Grammar.NS, ruleindex : Int, span : (Int, Int), rhs : Vector[ParseTree], input : Domain.V, output : Domain.V) extends ParseTree {
    lazy val hasAmbiguities = rhs.exists(_.hasAmbiguities)
    def ambiguities = rhs.toList.flatMap(_.ambiguities)
    lazy val countTrees : Int = {
      var c : Int = 1
      for (tree <- rhs) c *= tree.countTrees
      c
    }
  }

  final case class TerminalNode(symbol : Grammar.TS, span : (Int, Int), input : Domain.V, output : Domain.V) extends ParseTree {
    def hasAmbiguities = false
    def ambiguities = List()
    def countTrees : Int = 1
  }

  def label[CHAR](grammar : Grammar[CHAR], tree : ParseTree) : String = {
    val name = grammar.nameOf(tree.symbol)
    if (tree.input != Domain.V.NIL || tree.output != Domain.V.NIL) {
      name + "("+tree.input+","+tree.output+")"
    } else name
  }

  type Path = Vector[TerminalNode]
  type Paths = Set[Path]
  type Trees = Vector[ParseTree]

  private def combinePaths(A : Paths, B : Paths) : Paths = {
    var result : Paths = Set()
    for (a <- A) {
      for (b <- B) {
        result = result + (a ++ b)
      }
    }
    result
  }

  def collectPaths(tree : ParseTree) : Paths = {
    tree match {
      case node : AmbiguousNode =>
        var paths : Paths = Set()
        for (alternative <- node.alternatives) {
          paths = paths ++ collectPaths(alternative)
        }
        paths
      case node : TerminalNode => Set(Vector(node))
      case node : NonterminalNode =>
        var paths : Paths = Set(Vector())
        for (child <- node.rhs) {
          val childPaths = collectPaths(child)
          paths = combinePaths(paths, childPaths)
        }
        paths
    }
  }

  private def appendTrees(A : Vector[Vector[ParseTree]], B : Trees) : Vector[Vector[ParseTree]] = {
    var result : Vector[Vector[ParseTree]] = Vector()
    for (a <- A) {
      for (b <- B) {
        result = result :+ (a :+ b)
      }
    }
    result
  }  

  def collectTrees(tree : ParseTree) : Trees = {
    tree match {
      case node : AmbiguousNode =>
        var trees : Trees = Vector()
        for (alternative <- node.alternatives) {
          trees = trees ++ collectTrees(alternative)
        }
        trees
      case node : TerminalNode => 
        Vector(node)
      case node : NonterminalNode =>
        var trees : Vector[Vector[ParseTree]] = Vector(Vector())
        for (child <- node.rhs) {
          val childTrees = collectTrees(child)
          trees = appendTrees(trees, childTrees)
        }
        trees.map (rhs => NonterminalNode(node.symbol, node.ruleindex, node.span, rhs, node.input, node.output))
    }
  }

  def printPath[CHAR](grammar : Grammar[CHAR], input : Input[CHAR], path : Path) : String = {
    def printTerminalNode(node : TerminalNode) : String = {
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