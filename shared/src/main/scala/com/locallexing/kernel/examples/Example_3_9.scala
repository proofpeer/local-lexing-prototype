package com.locallexing.kernel.examples

final object Example_3_9 {

  import com.locallexing.kernel._
  import Grammar._
  import GrammarUtils._
  import LexingUtils._

  val S = NS(0)

  val a = TS(0)
  val b = TS(1)

  val grammar = Grammar[Char](
    Vector(nonterminal("S", rule(a, b), rule(b, a))),
    Vector(
      terminal("a", epsilon),
      terminal("b", epsilon)),
    unbiasedSelector)

  def main(args : Array[String]) {
    Examples.run("Example_3_9", grammar, "")
  } 

  def config : Configuration[Char] = Configuration.make(grammar)

}



