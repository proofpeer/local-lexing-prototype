package net.proofpeer.locallexing.examples.simple

import net.proofpeer.locallexing.kernel._
import Grammar._
import Examples._
import LexingUtils._

final object Example_3_2 {


  val S = NS(0)
  val T = NS(1)

  val t1 = TS(0)

  val grammar = mkGrammar(
    Vector(
      nonterminal("S", rule(S, T), rule()),
      nonterminal("T", rule(t1))),
    Vector(
      terminal("t1", repeat(char('a')))),
    unbiasedSelector)

  def main(args : Array[String]) {
    Examples.run("Example_3_2", grammar, "aa")
  } 

}