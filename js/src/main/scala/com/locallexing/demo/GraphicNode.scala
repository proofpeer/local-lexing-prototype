package com.locallexing.demo

import scala.scalajs.js

object DOM {

  val document = js.Dynamic.global.document

  def createElem(elem : String, attributes : (String, Any)*) : js.Dynamic = {
    val x = document.createElement(elem)
    for ((n, v) <- attributes)
      x.setAttribute(n, v.toString)
    return x
  }

  def textElem(t : String) : js.Dynamic = document.createTextNode(t)

}

object GraphicNode {

  type Cache = Map[String, Any]

  type Elem = js.Dynamic

  val document = js.Dynamic.global.document

  def createElem(elem : String, attributes : (String, Any)*) : Elem = {
    val x = document.createElementNS("http://www.w3.org/2000/svg", elem)
    for ((n, v) <- attributes)
      x.setAttribute(n, v.toString)
    return x
  }

  def createHiddenElem(name : String, attributes : (String, Any)*) : Elem = {
    val elem = createElem(name, attributes : _*)
    elem.setAttribute("visibility", "hidden")
    elem
  }

  def textElem(t : String) : Elem = document.createTextNode(t)

  def bbox(elem : Elem) : (Double, Double, Double, Double) = {
    val box = elem.getBBox()
    (box.x.asInstanceOf[Double], box.y.asInstanceOf[Double], 
      box.width.asInstanceOf[Double], box.height.asInstanceOf[Double])
  }

}

trait GraphicNode {

  import GraphicNode._

  def measure(svg : Elem) : (Double, Double, Cache)

  def paint(svg : Elem, cache : Cache, x : Double, y : Double)

}

class TextNode(TEXT : String, fontfamily : String = null, fontstyle : String = null, color : String = null) extends GraphicNode {

  import GraphicNode._

  def measure(svg : Elem) : (Double, Double, Cache) = {
    val text = createHiddenElem("text", "x" -> "0", "y" -> "0")
    if (fontfamily != null) text.setAttribute("font-family", fontfamily)
    if (fontstyle != null) text.setAttribute("font-style", fontstyle)
    if (color != null) text.setAttribute("fill", color)
    text.appendChild(textElem(TEXT))
    svg.appendChild(text)
    val (x, y, width, height) = bbox(text)
    svg.removeChild(text)
    (width, height, Map("x" -> x, "y" -> y))
  }

  def paint(svg : Elem, cache : Cache, x : Double, y : Double) {
    val dx = cache("x").asInstanceOf[Double]
    val dy = cache("y").asInstanceOf[Double]
    val text = createElem("text", "x" -> (x - dx), "y" -> (y - dy))
    if (fontfamily != null) text.setAttribute("font-family", fontfamily)
    if (fontstyle != null) text.setAttribute("font-style", fontstyle)
    if (color != null) text.setAttribute("fill", color)
    text.appendChild(textElem(TEXT))
    svg.appendChild(text)
  }

}

class EnclosingEllipse(innerNode : GraphicNode, fillColor : String = "#EFEFEF", strokeColor : String = "black", 
  inc : Double = 5) extends GraphicNode {

  import GraphicNode._

  def measure(svg : Elem) : (Double, Double, Cache) = {
    val (dx, dy, cache) = innerNode.measure(svg)
    val ry = dy / Math.sqrt(2) + inc
    val rx = Math.max(dx / Math.sqrt(2) + inc, ry)
    val ellipse = createHiddenElem("ellipse", "cx" -> "0", "cy" -> "0", "rx" -> rx, "ry" -> ry,
      "fill" -> fillColor, "stroke" -> strokeColor)
    svg.appendChild(ellipse)
    val (x, y, width, height) = bbox(ellipse)
    svg.removeChild(ellipse)
    (width, height, Map("text.cache" -> cache, "dx" -> dx, "dy" -> dy, "rx" -> rx, "ry" -> ry, 
      "width" -> width, "height" -> height, "x" -> x, "y" -> y))
  }

  def paint(svg : Elem, cache : Cache, x : Double, y : Double) {
    def get(name : String) : Double = cache(name).asInstanceOf[Double]
    val dx = get("dx")
    val dy = get("dy") 
    val rx = get("rx")
    val ry = get("ry")
    val width = get("width")
    val height = get("height")
    val px = get("x")
    val py = get("y")
    val ellipse = createElem("ellipse", "cx" -> (x - px), "cy" -> (y - py), "rx" -> rx, "ry" -> ry,
      "fill" -> fillColor, "stroke" -> strokeColor)
    svg.appendChild(ellipse)
    innerNode.paint(svg, cache("text.cache").asInstanceOf[Cache], 
      x + (width-dx) / 2, y + (height - dy) / 2)
  }

}

class EnclosingRect(innerNode : GraphicNode, fillColor : String = "#EFEFEF", strokeColor : String = "black", 
  inc : Double = 10, landscape : Option[Boolean] = None) extends GraphicNode {

  import GraphicNode._

  def measure(svg : Elem) : (Double, Double, Cache) = {
    val (dx, dy, cache) = innerNode.measure(svg)
    var h = dy + inc 
    var w = dx + inc
    landscape match {
      case None =>
      case Some(true) => w = Math.max(h, w)
      case Some(false) => h = Math.max(h, w)
    }
    val rect = createHiddenElem("rect", "x" -> "0", "y" -> "0", "width" -> w, "height" -> h,
      "fill" -> fillColor, "stroke" -> strokeColor)
    svg.appendChild(rect)
    val (x, y, width, height) = bbox(rect)
    svg.removeChild(rect)
    (width, height, Map("text.cache" -> cache, "dx" -> dx, "dy" -> dy, "w" -> w, "h" -> h, 
      "width" -> width, "height" -> height, "x" -> x, "y" -> y))
  }

  def paint(svg : Elem, cache : Cache, x : Double, y : Double) {
    def get(name : String) : Double = cache(name).asInstanceOf[Double]
    val dx = get("dx")
    val dy = get("dy")
    val width = get("width")
    val height = get("height")
    val w = get("w")
    val h = get("h")
    val px = get("x")
    val py = get("y")
    val rect = createElem("rect", "x" -> (x-px), "y" -> (y-py), "width" -> w, "height" -> h,
      "fill" -> fillColor, "stroke" -> strokeColor)
    svg.appendChild(rect)
    innerNode.paint(svg, cache("text.cache").asInstanceOf[Cache], 
      x + (width-dx) / 2, y + (height - dy) / 2)
  }

}

class TokenNode(terminalName : String, text : String, error : Boolean = false) extends GraphicNode {

  import GraphicNode._

  val ygap = 6
  val incx = 10

  def characterBox(text : String) : TextNode = {
    val color = (if (error) "red" else "black")
    if (text == "") new TextNode("ε", null, "italic", color)
    else new TextNode(text, "monospace", null, color)
  }

  def measure(svg : Elem) : (Double, Double, Cache) = {
    val box1 = characterBox(text)
    val box2 = new TextNode(terminalName, null, "italic")
    val (dx1, dy1, cache1) = box1.measure(svg)
    val (dx2, dy2, cache2) = box2.measure(svg)
    val dy = dy1 + dy2 + ygap
    val dx = Math.max(dx1, dx2) + incx
    (dx, dy, Map("cache1" -> cache1, "cache2" -> cache2, "dx1" -> dx1, "dy1" -> dy1, "dx2" -> dx2, "dy2" -> dy2))
  }

  def paint(svg : Elem, cache : Cache, x : Double, y : Double) {
    def get(name : String) : Double = cache(name).asInstanceOf[Double]
    val cache1 = cache("cache1").asInstanceOf[Cache]
    val cache2 = cache("cache2").asInstanceOf[Cache]
    val dx1 = get("dx1")
    val dy1 = get("dy1")
    val dx2 = get("dx2")
    val dy2 = get("dy2")
    val dy = dy1 + dy2 + ygap
    val dx = Math.max(dx1, dx2) + incx

    characterBox(text).paint(svg, cache1, x + (dx - dx1) / 2, y)
    val color = (if (error) "red" else "black")
    new TextNode(terminalName, null, "italic", color).paint(svg, cache2, x + (dx - dx2) / 2, y + dy1 + ygap)

    val liney = y + dy1 + ygap / 2

    val line = createElem("line", "x1" -> x, "x2" -> (x + dx), "y1" -> liney, "y2" -> liney,
      "stroke" -> color)

    svg.appendChild(line)
  }

  def lineY(cache : Cache) : Double = {
    val dy1 = cache("dy1").asInstanceOf[Double]
    dy1 + ygap / 2
  }

}
