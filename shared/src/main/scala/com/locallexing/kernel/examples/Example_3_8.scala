package com.locallexing.kernel.examples

final object Example_3_8 {

  import com.locallexing.kernel._
  import Grammar._
  import GrammarUtils._
  import LexingUtils._

  val Expr = NS(0)
  val Sum = NS(1)
  val Mul = NS(2)
  val Atom = NS(3)

  val num = TS(0)
  val id = TS(1)
  val mul = TS(2)
  val plus = TS(3)
  val left = TS(4)
  val right = TS(5)

  val e_atom = TS(6)
  val e_right = TS(7)
  val e_superfluous = TS(8)

  val NONTERMINALS = 
    Vector(
      nonterminal("Expr", rule(Sum)),
      nonterminal("Sum", rule(Sum, plus, Mul), rule(Mul)),
      nonterminal("Mul", rule(Mul, mul, Atom), rule(Mul, Atom), rule(Atom), rule(Mul, e_superfluous)),
      nonterminal("Atom", 
        rule(left, Sum, right), rule(num), rule(id),
        rule(e_atom), rule(left, Sum, e_right))
    )

  val TERMINALS : Vector[Terminal[Char]] = 
    Vector(
      terminal("num", repeat1(digit)),
      terminal("id", concat(letter, repeat(or(digit, letter)))),
      terminal("mul", char('*')),
      terminal("plus", char('+')),
      terminal("left", char('(')),
      terminal("right", char(')')),
      terminal("e-atom", epsilon),
      terminal("e-right", epsilon),
      terminal("e-superfluous", char(')'))
    )

  /** Alphabet: letter digit + * ( )  */

  def ERRORS(ts1 : TS, len1 : Int, ts2 : TS, len2 : Int) : Boolean = {
    def is_error(ts : TS) : Boolean = {ts == e_atom || ts == e_superfluous || ts == e_right}
    is_error(ts1) && !is_error(ts2)
  }
    
  val grammar = Grammar(NONTERMINALS, TERMINALS, orderSelector(ERRORS))
  
  def main(args : Array[String]) {
    Examples.run("Example_3_8_1", grammar, "2(a*+))+(1", true)
    Examples.run("Example_3_8_2", grammar, "(a+*b", false)
    Examples.run("Example_3_8_3", grammar, "(a*+", false)
    Examples.run("Example_3_8_4", grammar, "(a+*)", false)
    Examples.run("Example_3_8_5", grammar, "a+", false)
    Examples.run("Example_3_8_6", grammar, ")", false)
    Examples.run("Example_3_8_7", grammar, "(a*+", false)
    Examples.run("Example_3_8_8", grammar, "(a*+*+", false)
    Examples.run("Example_3_8_9", grammar, "))6(a*+*+", false)
  } 

  def config : Configuration[Char] = Configuration.make(grammar, Set(e_atom, e_right, e_superfluous))

}



