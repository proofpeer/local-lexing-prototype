package net.proofpeer.locallexing.examples.simple

import net.proofpeer.locallexing.kernel._
import Grammar._
import Examples._
import LexingUtils._

final object Example_3_9 {

  val S = NS(0)

  val a = TS(0)
  val b = TS(1)

  val grammar = mkGrammar(
    Vector(nonterminal("S", rule(a, b), rule(b, a))),
    Vector(
      terminal("a", epsilon),
      terminal("b", epsilon)),
    unbiasedSelector)

  def main(args : Array[String]) {
    Examples.run("Example_3_9", grammar, "")
  } 

}



