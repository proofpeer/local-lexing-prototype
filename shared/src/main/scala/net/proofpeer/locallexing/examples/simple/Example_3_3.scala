package net.proofpeer.locallexing.examples.simple

import net.proofpeer.locallexing.kernel._
import Grammar._
import Examples._
import LexingUtils._

final object Example_3_3 {

  val S = NS(0)
  val A = NS(1)
  val E = NS(2)

  val plus = TS(0)
  val minus = TS(1)
  val id = TS(2)
  val symbol = TS(3)

  val grammar = mkGrammar(
    Vector(
      nonterminal("S", rule(S, plus, A), rule(S, minus, A), rule(A)),
      nonterminal("A", rule(A, E), rule(E)),
      nonterminal("E", rule(id), rule(symbol))),
    Vector(
      terminal("plus", char('+')),
      terminal("minus", char('-')),
      terminal("id", repeat1(letter)),
      terminal("symbol", repeat1(or(letter, char('-'))))),
    unbiasedSelector)

  def main(args : Array[String]) {
    Examples.run("Example_3_3", grammar, "a-b+c")
  } 

}
