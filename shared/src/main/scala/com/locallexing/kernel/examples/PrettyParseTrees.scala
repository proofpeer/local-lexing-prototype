package com.locallexing.kernel.examples

import com.locallexing.kernel._

sealed trait Node {
  def id : Int
  def isTerminal : Boolean = false
  def isNonterminal : Boolean = false
  def isAlternative : Boolean = false
}

object Node {
  case class Terminal(id : Int, terminal : String, text : String, from : Int, to : Int) extends Node {
    override def isTerminal = true
  }
  case class Nonterminal(id : Int, nonterminal : String, ambiguous : Boolean, from : Int, to : Int) extends Node {
    override def isNonterminal = true
  }
  case class Alternative(id : Int, nonterminal : String, choice : Int) extends Node {
    override def isAlternative = true
  }
}

class Graph[CHAR](input : Input[CHAR]) {

  private var nextId : Int = 0
  private var cacheTerminals : Map[(String, Int, Int), Node.Terminal] = Map()
  private var cacheNonterminals : Map[(String, Int, Int), Node.Nonterminal] = Map()
  private var _nodes : Map[Int, Node] = Map()
  private var _edges : Map[Int, Set[Int]] = Map()
  private var _layers : Vector[Vector[Int]] = Vector()

  private def getNextId() : Int = {
    val id = nextId 
    nextId += 1
    id
  }

  def addTerminal(terminal : String, from : Int, to : Int) : Node.Terminal = {
    val key = (terminal, from, to)
    cacheTerminals.get(key) match {
      case Some(terminalNode) => terminalNode
      case None =>
        val text = input.print(from, to - from)
        val terminalNode = Node.Terminal(getNextId(), terminal, text, from, to)
        cacheTerminals += (key -> terminalNode)
        _nodes += (terminalNode.id -> terminalNode)
        terminalNode
    }
  }

  def addNonterminal(nonterminal : String, ambiguous : Boolean, from : Int, to : Int) : Node.Nonterminal = {
    val key = (nonterminal, from, to)
    cacheNonterminals.get(key) match {
      case Some(nonterminalNode) => nonterminalNode
      case None =>
        val nonterminalNode = Node.Nonterminal(getNextId(), nonterminal, ambiguous, from, to)
        cacheNonterminals += (key -> nonterminalNode)
        _nodes += (nonterminalNode.id -> nonterminalNode)
        nonterminalNode
    }    
  }

  def addAlternative(nonterminal : String, choice : Int) : Node.Alternative = {
    val node = Node.Alternative(getNextId(), nonterminal, choice)
    _nodes += (node.id -> node)
    node
  }

  def addEdge(fromId : Int, toId : Int) {
    _edges.get(fromId) match {
      case None => _edges += (fromId -> Set(toId))
      case Some(ids) => _edges += (fromId -> (ids + toId))
    }
  }

  def edges : Map[Int, Set[Int]] = _edges

  def nodes : Map[Int, Node] = _nodes

  def layers : Vector[Vector[Int]] = _layers

  def addLayer(layer : Vector[Int]) {
    _layers = _layers :+ layer
  }

}

object PrettyParseTrees {

  import ParseTree._

  def mkGraph[CHAR](grammar : Grammar[CHAR], input : Input[CHAR], tree : ParseTree) : Graph[CHAR] = {
    val graph = new Graph(input)
    def nameOf(tree : ParseTree) : String = ParseTree.label(grammar, tree)
    def registerLayer(layer : Vector[Int]) {
      graph.addLayer(layer)
    }
    def mk(tree : ParseTree, alternative : Option[Int]) : Node = {
      tree match {
        case node : NonterminalNode => 
          val from = 
            alternative match {
              case None => graph.addNonterminal(nameOf(node), false, node.span._1, node.span._2)
              case Some(choice) => graph.addAlternative(nameOf(node), choice)
            }
          var layer : Vector[Int] = Vector()
          for (subtree <- node.rhs) {
            val to = mk(subtree, None)
            graph.addEdge(from.id, to.id)
            layer = layer :+ to.id
          }
          registerLayer(layer)
          from
        case node : TerminalNode =>
          graph.addTerminal(nameOf(node), node.span._1, node.span._2)
        case node : AmbiguousNode =>
          val from = graph.addNonterminal(nameOf(node), true, node.span._1, node.span._2)
          var choice = 1
          var layer : Vector[Int] = Vector()
          for (alternative <- node.alternatives) {
            val to = 
              if (alternative.rhs.length != 1)
                mk(alternative, Some(choice))
              else 
                mk(alternative.rhs(0), None)
            choice = choice + 1
            graph.addEdge(from.id, to.id)
            layer = layer :+ to.id
          }
          registerLayer(layer)
          from
      }
    }
    mk(tree, None)
    graph
  }

  /**
   * To render the output as LaTeX: 
   *
   *     dot2tex --preproc parsetree.dot | dot2tex > parsetree.tex
   *
   * where the output string is supposed to be contained in the file parsetree.dot
   */
  def asDot[CHAR](graphname : String, graph : Graph[CHAR]) : String = {
    val s = new StringBuilder()
    def nodename(id : Int) = "LR_" + id
    def nodedef(node : Node) : String = {
      val name = nodename(node.id)
      val bslash = "\\\\"
      def text(s : String) = "{" + bslash + "textsl{" + s + "}}"
      def verbatim(s : String) = {
        if (s == "") "\\varepsilon" else "{" + bslash + "texttt{" + s + "}}"
      }
      val plain = "none width=0 height=0"
      val (shape, label) = 
        node match {
          case node : Node.Terminal => 
            val l = bslash + "dfrac" + verbatim(node.text) + text(node.terminal) 
            (plain, l)
          case node : Node.Nonterminal => 
            (plain, text(node.nonterminal))
          case node : Node.Alternative => 
            (plain, text(node.nonterminal))
        }
      name + "[shape=" + shape + ",label=\"" + label + "\"]"
    }
    def listNodes() {
      for ((_, node) <- graph.nodes) {
        s.append("  ")
        s.append(nodedef(node))
        s.append(";\n")
      }
    }
    def isAmbiguous(id : Int) : Boolean = {
      graph.nodes(id) match {
        case node : Node.Nonterminal => node.ambiguous
        case _ => false
      }
    }
    def listEdges() {
      for ((src, targets) <- graph.edges) {
        val ambiguous = isAmbiguous(src)
        for (target <- targets) {
          s.append("  ")
          s.append(nodename(src))
          s.append(" -> ")
          s.append(nodename(target))
          if (ambiguous) {
            s.append(" [style=dashed,dir=none]")
          }
          s.append(";\n")
        } 
      }
    }
    def listLayers() {
      for (layer <- graph.layers) {
        if (layer.size >= 2) {
          s.append("{\n")
          s.append("rank=same;\n")
          s.append(nodename(layer(0)))
          for (i <- 1 until layer.size) {
            s.append("->" + nodename(layer(i)))
          }
          s.append("[style=invis];\n")
          s.append("rankdir=LR;\n")
          s.append("}\n")
        }
      }
    }
    s.append("digraph " + graphname + " {\n")
    s.append("  rankdir=TB;\n")
    s.append("  graph [ordering=\"out\"];\n")
    s.append("  node [texmode=\"math\",lblstyle=\"font=\\LARGE\"];\n")
    listNodes()
    listEdges()
    listLayers()
    s.append("}\n")
    s.toString
  }

}
