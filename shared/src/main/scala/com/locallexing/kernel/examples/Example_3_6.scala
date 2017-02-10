package com.locallexing.kernel.examples

final object Example_3_6 {

  import com.locallexing.kernel._
  import Grammar._
  import GrammarUtils._
  import LexingUtils._

  val S = NS(0)
  val A = NS(1)
  val E = NS(2)

  val plus = TS(0)
  val minus = TS(1)
  val id = TS(2)
  val symbol = TS(3)

  def LESS(ts1 : TS, len1 : Int, ts2 : TS, len2 : Int) : Boolean = {
    len1 < len2 || (len1 == len2 && ts1 == symbol && ts2 == id)
  }

  val grammar = Grammar(
    Vector(
      nonterminal("S", rule(S, plus, A), rule(S, minus, A), rule(A)),
      nonterminal("A", rule(A, E), rule(E)),
      nonterminal("E", rule(id), rule(symbol))),
    Vector(
      terminal("plus", char('+')),
      terminal("minus", char('-')),
      terminal("id", repeat1(letter)),
      terminal("symbol", repeat1(or(letter, char('-'))))),
    orderSelector(LESS))

  def main(args : Array[String]) {
    Examples.run("Example_3_6", grammar, "a-b+c")
  } 

  def config : Configuration[Char] = Configuration.make(grammar)

}



