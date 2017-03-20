package net.proofpeer.locallexing.kernel

/** 
  The start nonterminal has always index 0. 
  */
final case class Grammar[CHAR, P](
  nonterminals : Vector[Grammar.Nonterminal[P]],
  terminals : Vector[Grammar.Terminal[CHAR, P]],
  startParam : P,
  selector : Grammar.Selector[CHAR, P]) 
{
  import Grammar._

  def getOpt(n : NS) : Option[Nonterminal[P]] = {
    if (n.index >= 0 && n.index < nonterminals.size) Some(nonterminals(n.index)) else None
  }

  def get(n : NS) : Nonterminal[P] = getOpt(n).get

  def getOpt(t : TS) : Option[Terminal[CHAR, P]] = {
    if (t.index >= 0 && t.index < terminals.size) Some(terminals(t.index)) else None
  }

  def get(t : TS) : Terminal[CHAR, P] = getOpt(t).get

  def nameOf(symbol : Symbol) : String = {
    symbol match {
      case ns : NS => get(ns).name
      case ts : TS => get(ts).name
    }
  }
} 

final object Grammar {

  sealed trait Symbol 
  final case class NS(index : Int) extends Symbol
  final case class TS(index : Int) extends Symbol

  final case class Nonterminal[P](name : String, rules : Vector[Rule[P]])

  final case class Terminal[CHAR, P](name : String, lexer : Lexer[CHAR, P])

  trait Environment[P] {
    def size : Int // the number of symbols on the right hand side of the rule
    def dot : Int  // the position of the dot
    /** The nonterminal on the left hand side of a rule has index 0, the right hand side symbols start at index 1 */
    def inputAt(index : Int) : P // index is valid iff (index <= dot)
    def outputAt(index : Int) : P // index is valid iff (index > 0 && index <= dot)
  }

  type EnvFun[P] = Environment[P] => Option[P]

  final case class Rule[P](out : EnvFun[P], rhs : Vector[(Symbol, EnvFun[P])])

  trait Lexer[CHAR, P] {
    def lex(input : Input[CHAR], startPosition : Int, param : P) : Set[(Int, P)]
  }

  type Tokens[P] = Map[(TS, P), Set[(Int, P)]]

  trait Selector[CHAR, P] {
    def select(input : Input[CHAR], startPosition : Int, A : Tokens[P], B : Tokens[P]) : Tokens[P]
  }

  /** Checks if a grammar is wellformed. */
  def check[CHAR, P](g : Grammar[CHAR, P]) {

    case class Err(s : String) extends RuntimeException

    def fail[T](s : String) {
      throw Err("error checking grammar: " + s)
    }
    if (g.nonterminals.size == 0) fail("start nonterminal expected")

    def checkRule(rule : Rule[P]) {
      for ((symbol, _) <- rule.rhs) {
        symbol match {
          case symbol : NS => 
            g.getOpt(symbol) match {
              case Some(nonterminal) => // ok
              case None => fail("unknown nonterminal " + symbol)
            }
          case symbol : TS => 
            g.getOpt(symbol) match {
              case Some(terminal) => // ok
              case None => fail("unknown terminal " + symbol)
            }
        }
      }
    }

    var index = 0
    val numNonterminals = g.nonterminals.size
    while (index < numNonterminals) {
      val ns = NS(index)
      val n = g.nonterminals(index)
      var ruleindex = 0
      for (rule <- n.rules) {
        try {
          checkRule(rule)
        } catch {
          case Err(message) => fail("nonterminal " + index + ", rule " + ruleindex + ": " + message)
        }
        ruleindex += 1
      }
      index += 1
    }
  }

}
