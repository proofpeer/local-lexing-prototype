package com.locallexing.kernel

final object GrammarUtils {

  import Grammar._

  def rule(rhs : Symbol*) : Rule = 
    Rule(Domain.Expr.NIL, Domain.Expr.BOOL(true), rhs.map(sym => (sym, Domain.Expr.NIL)).toVector)

  def nonterminal(name : String, rules : Rule*) : Nonterminal = 
    Nonterminal(name, Domain.T.UNIT, Domain.T.UNIT, rules.toVector)

  def terminal[CHAR](name : String, lexer : Lexer[CHAR]) : Terminal[CHAR] =
    Terminal(name, Domain.T.UNIT, Domain.T.UNIT, lexer)

  def makeParser[CHAR](grammar : Grammar[CHAR]) : Earley[CHAR] = {
    Grammar.check(grammar)
    val kernel = new Earley.Kernel(grammar)
    new Earley(kernel)
  }

}