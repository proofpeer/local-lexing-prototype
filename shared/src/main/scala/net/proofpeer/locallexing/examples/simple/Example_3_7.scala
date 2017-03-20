package net.proofpeer.locallexing.examples.simple

import net.proofpeer.locallexing.kernel._
import Grammar._
import Examples._
import LexingUtils._

final object Example_3_7 {

  val Expr = NS(0)
  val Mul = NS(1)
  val Cast = NS(2)
  val Deref = NS(3)
  val Type = NS(4)

  val typeid = TS(0)
  val id = TS(1)
  val asterisk = TS(2)
  val left = TS(3)
  val right = TS(4)

  val grammar = mkGrammar(
    Vector(
      nonterminal("Expr", rule(Mul), rule(Cast), rule(Deref), rule(id), rule(left, Expr, right)),
      nonterminal("Mul", rule(Expr, asterisk, Expr)),
      nonterminal("Cast", rule(left, Type, right, Expr)),
      nonterminal("Deref", rule(asterisk, Expr)),
      nonterminal("Type", rule(typeid))),      
    Vector(
      terminal("typeid", repeat1(letter)),
      terminal("id", repeat1(letter)),
      terminal("asterisk", char('*')),
      terminal("left", char('(')),
      terminal("right", char(')'))),
    unbiasedSelector)

  def main(args : Array[String]) {
    Examples.run("Example_3_7", grammar, "(a)*b")
  } 

}



