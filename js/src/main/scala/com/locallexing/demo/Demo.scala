package com.locallexing.demo

import scala.scalajs.js.JSApp
import scala.scalajs.js

import com.locallexing.kernel._
import com.locallexing.kernel.examples.Configuration

class WrapLayoutNode(nodes : Vector[GraphicNode], maxwidth : Double, 
  vgap : Double, hgap : Double, inset : Double, alignY : (GraphicNode, GraphicNode.Cache) => Double) extends GraphicNode 
{

  import GraphicNode._

  val LINE = "line"
  val LINES = "lines"
  val NODE = "node"

  def measure(svg : Elem) : (Double, Double, Cache) = {
    var currentwidth = 0.0
    var currentheight = 0.0
    var currentlineY = 0.0
    var currentbottomheight = 0.0
    var currentlinewidth = 0.0
    var node_index = 0
    var currentCache : Cache = Map()
    var line = 0
    def newline() {
      if (currentlinewidth > currentwidth) {
        currentwidth = currentlinewidth
      }
      val h = currentlineY + currentbottomheight
      if (currentheight == 0) 
        currentheight = h
      else
        currentheight = currentheight + vgap + h
      val lineinfo = (node_index, currentheight - h, currentlineY)
      currentCache = currentCache + ((LINE + line) -> lineinfo)
      currentlineY = 0
      currentbottomheight = 0
      currentlinewidth = 0
      line = line + 1
    }
    for (node <- nodes) {
      val (dx, dy, cache) = node.measure(svg)
      val lineY = alignY(node, cache)
      if (currentlinewidth > 0 && currentlinewidth + hgap + dx > maxwidth) newline()
      if (currentlinewidth > 0) currentlinewidth = currentlinewidth + hgap
      else if (line > 0) currentlinewidth += inset
      currentlinewidth += dx
      if (lineY > currentlineY) currentlineY = lineY
      val bottomheight = dy - lineY
      if (bottomheight > currentbottomheight) currentbottomheight = bottomheight
      val nodeinfo = (cache, currentlinewidth - dx)
      currentCache = currentCache + ((NODE + node_index) -> nodeinfo)
      node_index = node_index + 1
    }
    newline()
    currentCache = currentCache + (LINES -> line)
    (currentwidth, currentheight, currentCache)
  }

  def paint(svg : Elem, cache : Cache, x : Double, y : Double) {
    val lines = cache(LINES).asInstanceOf[Int]
    var node_index = 0
    for (line <- 0 until lines) {
      val (numNodes, top, maxlineY) = cache(LINE + line).asInstanceOf[(Int, Double, Double)]
      while (node_index < numNodes) {
        val (node_cache, node_x) = cache(NODE + node_index).asInstanceOf[(Cache, Double)]
        val node = nodes(node_index)
        val lineY = alignY(node, node_cache)
        val node_y = top + (maxlineY - lineY) 
        node.paint(svg, node_cache, x + node_x, y + node_y)
        node_index = node_index + 1
      }
    }
  }

}

class GroupNode(inner : GraphicNode) extends GraphicNode {

  import GraphicNode._


  def measure(svg : Elem) : (Double, Double, Cache) = {
    inner.measure(svg)
  }

  def group(svg : Elem, cache : Cache, x : Double, y : Double) : Elem = {
    val g = createElem("g")
    inner.paint(g, cache, x, y)
    svg.appendChild(g)
    g
  }

  def paint(svg : Elem, cache : Cache, x : Double, y : Double) {
    group(svg, cache, x, y)
  }


}


/*class PathNode[CHAR](config : Configuration[CHAR], input : Input[CHAR], path : ParseTree.Path, maxwidth : Double) 
  extends GraphicNode 
{
  import GraphicNode._

  val vertical_gap = 3
  val horizontal_gap = 3

  def mkToken(terminalNode : ParseTree.TerminalNode) : TokenNode = {
    val terminalName = ParseTree.label(config.grammar, terminalNode)
    val text = input.print(terminalNode.span._1, terminalNode.span._2 - terminalNode.span._1)
    new TokenNode(terminalName, text)
  }


} */

class TreeNode[CHAR](config : Configuration[CHAR], input : Input[CHAR], tree : ParseTree) extends GraphicNode {
  import GraphicNode._

  val vertical_gap = 16
  val horizontal_gap = 8

  def measure(svg : Elem) : (Double, Double, Cache) = {
    tree match {
      case _: ParseTree.AmbiguousNode => throw new RuntimeException("internal error")
      case terminalNode: ParseTree.TerminalNode =>
        val terminalName = ParseTree.label(config.grammar, terminalNode)
        val text = input.print(terminalNode.span._1, terminalNode.span._2 - terminalNode.span._1)
        val error = config.errorTerminals.contains(terminalNode.symbol)
        val token = new EnclosingRect(new TokenNode(terminalName, text, error))
        token.measure(svg)
      case nonterminalNode : ParseTree.NonterminalNode =>
        val topNode = Demo.NonterminalNode(ParseTree.label(config.grammar, nonterminalNode))
        val (top_dx, top_dy, top_cache) = topNode.measure(svg)
        var childrenHeight = 0.0
        var childrenWidth = 0.0
        var childrenInfo : Vector[(Double, Double, Double, Double, Cache)] = Vector()
        var childIndex = 0
        for (child <- nonterminalNode.rhs) {
          val childNode = new TreeNode(config, input, child)
          val (child_dx, child_dy, child_cache) = childNode.measure(svg)
          if (childIndex > 0) childrenWidth += horizontal_gap
          childrenWidth += child_dx
          childrenHeight = Math.max(childrenHeight, child_dy)
          val childInfo = (childrenWidth - child_dx, top_dy + horizontal_gap, child_dx, child_dy, child_cache)
          childrenInfo = childrenInfo :+ childInfo
          childIndex += 1
        }
        if (childrenWidth >= top_dx) {
          val topInfo = (childrenWidth / 2 - top_dx / 2, top_dx, top_dy, top_cache)
          val cache = Map("children" -> childrenInfo, "top" -> topInfo)
          (childrenWidth, top_dy + horizontal_gap + childrenHeight, cache)
        } else {
          val topInfo = (0, top_dx, top_dy, top_cache)
          val dx = (top_dx - childrenWidth) / 2
          val updatedChildrenInfo = childrenInfo.map(info => (info._1 + dx, info._2, info._3, info._4, info._5))
          val cache = Map("children" -> updatedChildrenInfo, "top" -> topInfo)
          (top_dx, top_dy + horizontal_gap + childrenHeight, cache)
        }
    }
  }

  def paint(svg : Elem, cache : Cache, x : Double, y : Double) {
    tree match {
      case _ : ParseTree.AmbiguousNode => throw new RuntimeException("internal error")
      case terminalNode : ParseTree.TerminalNode =>
        val terminalName = ParseTree.label(config.grammar, terminalNode)
        val text = input.print(terminalNode.span._1, terminalNode.span._2 - terminalNode.span._1)
        val error = config.errorTerminals.contains(terminalNode.symbol)
        val token = new EnclosingRect(new TokenNode(terminalName, text, error))
        token.paint(svg, cache, x, y)
      case nonterminalNode : ParseTree.NonterminalNode =>
        val topNode = Demo.NonterminalNode(ParseTree.label(config.grammar, nonterminalNode))
        val (top_x, top_dx, top_dy, top_cache) = cache("top").asInstanceOf[(Double, Double, Double, Cache)]
        val x1 = x + top_x + top_dx / 2
        val y1 = y + top_dy / 2
        val childrenInfo = cache("children").asInstanceOf[Vector[(Double, Double, Double, Double, Cache)]]
        for (childIndex <- 0 until childrenInfo.size) {
          val (child_x, child_y, child_dx, child_dy, child_cache) = childrenInfo(childIndex)
          val x2 = x + child_x + child_dx / 2
          val y2 = y + child_y
          val line = createElem("line", "x1" -> x1, "y1" -> y1, "x2" -> x2, "y2" -> y2, "stroke" -> "black")
          svg.appendChild(line)
        }        
        topNode.paint(svg, top_cache, x + top_x, y)
        for (childIndex <- 0 until childrenInfo.size) {
          val (child_x, child_y, child_dx, child_dy, child_cache) = childrenInfo(childIndex)
          val childNode = new TreeNode(config, input, nonterminalNode.rhs(childIndex))
          childNode.paint(svg, child_cache, x + child_x, y + child_y)
        }
    }

  }


}



final object Demo extends JSApp {

  def NonterminalNode(nonterminal : String) = new EnclosingEllipse(new TextNode(nonterminal))

  def TerminalNode(terminal : String, text : String) = new EnclosingRect(new TokenNode(terminal, text))

  def PathNode[CHAR](config : Configuration[CHAR], input : Input[CHAR], path : ParseTree.Path, maxwidth : Double) : GraphicNode = {
    import GraphicNode._

    val vertical_gap = 3
    val horizontal_gap = 8

    def mkToken(terminalNode : ParseTree.TerminalNode) : TokenNode = {
      val terminalName = ParseTree.label(config.grammar, terminalNode)
      val text = input.print(terminalNode.span._1, terminalNode.span._2 - terminalNode.span._1)
      val error = config.errorTerminals.contains(terminalNode.symbol)
      new TokenNode(terminalName, text, error)
    }

    val nodes = path.map(mkToken _)

    def alignY(node : GraphicNode, cache : GraphicNode.Cache) : Double = {
      node.asInstanceOf[TokenNode].lineY(cache)
    }

    if (path.size > 0)
      new WrapLayoutNode(nodes, maxwidth, vertical_gap, horizontal_gap, 20, alignY _)
    else
      new TextNode("ε", null, "italic")
  }

  def handleSelectTree(treeIndex : Int) : () => Unit = {
    def f() : Unit = {
      if (treeIndex != State.selectedTree) {
        State.selectedTree = treeIndex
        State.show()
      }
    }
    f _
  }

  def computeTreesAndPaths(config : Configuration[Char], inputStr : String) {
    val input = new StringInput(inputStr)
    val result = config.parser.parse(input)
    result match {
      case Left(parseTree) => 
        val trees = ParseTree.collectTrees(parseTree)
        State.treesAndPaths = trees.map(tree => (tree, ParseTree.collectPaths(tree).head))
      case Right(errorAt) => 
        State.errorAt = errorAt
        State.treesAndPaths = Vector()
    }
  }

  def showPaths() {
    val svg = getById("showPaths")
    svg.innerHTML = ""
    val input = new StringInput(State.currentInputStr)
    if (State.treesAndPaths.size > 0) {
      var x = 60.0
      var y = 20.0
      var width = 0.0
      for (p <- 1 to State.treesAndPaths.size) {
        val bullet = new TextNode("" + p + ")")
        val (bulletdx, bulletdy, bulletcache) = bullet.measure(svg)
        bullet.paint(svg, bulletcache, x - 10 - bulletdx, y)
        val color = if (p == State.selectedTree + 1) "#f9f8b8" else "#efefef"
        val pathNode = new GroupNode(new EnclosingRect(PathNode(State.currentConfig, input, 
          State.treesAndPaths(p - 1)._2, 600), color))
        val (dx, dy, cache) = pathNode.measure(svg)
        width = Math.max(width, x + dx + 10)
        val g = pathNode.group(svg, cache, x, y)
        g.addEventListener("click", handleSelectTree(p-1))
        y = y + dy + 10
      }
      svg.setAttribute("width", "" + Math.ceil(width))
      svg.setAttribute("height", "" + Math.ceil(y))
    } else {
      val textNode = new TextNode("Parsing error found at position " + State.errorAt + ".")
      val (dx, dy, cache) = textNode.measure(svg)
      textNode.paint(svg, cache, 60, 20)
      svg.setAttribute("width", "" + Math.ceil(60 + dx + 20))
      svg.setAttribute("height", "" + Math.ceil(20 + dy + 20))
    }
  }

  def showGraph() {
    val svg = getById("showGraph")
    svg.innerHTML = ""
    if (State.treesAndPaths.size == 0) return
    val header = new TextNode("Parse tree " + (State.selectedTree + 1) + ")")
    val (header_dx, header_dy, header_cache) = header.measure(svg)
    header.paint(svg, header_cache, 10, 20)
    val graph = new TreeNode(State.currentConfig, new StringInput(State.currentInputStr),
      State.treesAndPaths(State.selectedTree)._1)
    val (graph_dx, graph_dy, graph_cache) = graph.measure(svg)
    graph.paint(svg, graph_cache, 10, 20 + header_dy + 5)
    svg.setAttribute("width", "" + (Math.max(header_dx, graph_dx) + 10 + 10))
    svg.setAttribute("height", "" + (header_dy + 20 + 5 + graph_dy + 20))
  }

  var InstalledExamples : Vector[(String, Configuration[Char])] = Vector()

  object State {
    var selectedTree : Int = -1
    var treesAndPaths : Vector[(ParseTree, ParseTree.Path)] = null
    var errorAt = -1
    var currentConfig : Configuration[Char] = null
    var currentInputStr : String = null

    def set() {
      val selector = getById("selectExample")
      val example = selector.value.asInstanceOf[String].toInt
      val (defaultInput, config) = InstalledExamples(example)
      if (currentInputStr == null) {
        currentInputStr = defaultInput
        getById("typeInput").value = currentInputStr.asInstanceOf[js.Any]
      }
      currentConfig = config
      currentInputStr = getById("typeInput").value.asInstanceOf[String]
      computeTreesAndPaths(currentConfig, currentInputStr) 
      selectedTree = 0
    }

    def show() {
      showPaths()
      showGraph()
    }
  }

  def installExamples() {
    def install(name : String, defaultInput : String, config : Configuration[Char]) {
      val selector = getById("selectExample")
      val option = DOM.createElem("option", "value" -> InstalledExamples.size.toString)
      option.appendChild(DOM.textElem(name))
      selector.appendChild(option)
      val e = (defaultInput, config)
      InstalledExamples = InstalledExamples :+ e
    }
    import com.locallexing.kernel.examples._
    install("Example 3.2", "aa", Example_3_2.config)
    install("Example 3.3", "a-b+c", Example_3_3.config)
    install("Example 3.4", "a-b+c", Example_3_4.config)
    install("Example 3.5", "a-b+c", Example_3_5.config)
    install("Example 3.6", "a-b+c", Example_3_6.config)
    install("Example 3.7", "(a)*b", Example_3_7.config)
    install("Example 3.8", "2(a*+))+(1", Example_3_8.config)
    //install("Example 3.9", "", Example_3_9.config)
  }

  def getById(id : String) : js.Dynamic = {
    GraphicNode.document.getElementById(id) 
  }

  def main(): Unit = {
    installExamples()
    State.set()
    State.show()
  }

  import js.annotation.JSExport

  @JSExport
  def onTypeInput(): Unit = {
    State.set()
    State.show()
  }

  @JSExport
  def onSelectExample(): Unit = {
    State.currentInputStr = null
    State.set()
    State.show()
  }

}
