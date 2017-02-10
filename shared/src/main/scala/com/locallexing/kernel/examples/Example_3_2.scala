package com.locallexing.kernel.examples

final object Example_3_2 {

  import com.locallexing.kernel._
  import Grammar._
  import GrammarUtils._
  import LexingUtils._

  val S = NS(0)
  val T = NS(1)

  val t1 = TS(0)

  val grammar = Grammar(
    Vector(
      nonterminal("S", rule(S, T), rule()),
      nonterminal("T", rule(t1))),
    Vector(
      terminal("t1", repeat(char('a')))),
    unbiasedSelector)

  def main(args : Array[String]) {
    Examples.run("Example_3_2", grammar, "aa")
  } 

  def config : Configuration[Char] = Configuration.make(grammar)

}



