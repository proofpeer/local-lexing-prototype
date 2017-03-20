package net.proofpeer.locallexing.examples.simple

import net.proofpeer.locallexing.kernel._
import Grammar._
import Examples._
import LexingUtils._

final object LoupVaillantEmptyRules {

  val A = NS(0)
  val B = NS(1)

  val grammar = mkGrammar(
    Vector(
      nonterminal("A", rule(), rule(B)),
      nonterminal("B", rule(A))),
    Vector(),
    unbiasedSelector)

  def main(args : Array[String]) {
    Examples.run("LoupVaillantEmptyRules", grammar, "")
  } 

}



