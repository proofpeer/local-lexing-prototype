package com.locallexing.kernel

/** 
  The start nonterminal has always index 0, and must have UNIT input and output type. 
  */
final case class Grammar[CHAR](
  nonterminals : Vector[Grammar.Nonterminal],
  terminals : Vector[Grammar.Terminal[CHAR]],
  selector : Grammar.Selector[CHAR]) 
{
  import Grammar._

  def getOpt(n : NS) : Option[Nonterminal] = {
    if (n.index >= 0 && n.index < nonterminals.size) Some(nonterminals(n.index)) else None
  }

  def get(n : NS) : Nonterminal = getOpt(n).get

  def getOpt(t : TS) : Option[Terminal[CHAR]] = {
    if (t.index >= 0 && t.index < terminals.size) Some(terminals(t.index)) else None
  }

  def get(t : TS) : Terminal[CHAR] = getOpt(t).get

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

  final case class Nonterminal(name : String, inT : Domain.T, outT : Domain.T, rules : Vector[Rule])

  final case class Terminal[CHAR](name : String, inT : Domain.T, outT : Domain.T, lexer : Lexer[CHAR])

  final case class Rule(out : Domain.Expr, guard : Domain.Expr, rhs : Vector[(Symbol, Domain.Expr)])

  trait Lexer[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, value : Domain.V) : Option[(Int, Domain.V)]
  }

  type Tokens = Map[(TS, Domain.V), (Int, Domain.V)]

  trait Selector[CHAR] {
    def select(input : Input[CHAR], startPosition : Int, A : Tokens, B : Tokens) : Tokens
  }

  /** Calculates the type of an expr and the minimal dot value for which this expr makes sense. */
  def typeExpr[CHAR](grammar : Grammar[CHAR], lhs: NS, rule : Rule, expr : Domain.Expr) : Option[(Int, Domain.T)] = {
    import Domain.{Expr => E}
    import Domain.{T => T}

    def typeTuple(args : Vector[E]) : Option[(Int, T)] = {
      var minDot = 0
      var types : Vector[T] = Vector()
      for (arg <- args) {
        computeType(arg) match {
          case None => return None
          case Some((dot, ty)) => 
            minDot = Math.max (minDot, dot)
            types = types :+ ty
        }
      }
      Some((minDot, T.TUPLE(types)))
    }

    def typeUnary(expr : E, t : T, r : T) : Option[(Int, Domain.T)] = {
      computeType(expr) match {
        case Some((dot, ty)) if ty == t => Some((dot, r))
        case None => None
      }
    }

    def typeBinary(expr1 : E, expr2 : E, t : T, r : T) : Option[(Int, T)] = {
      computeType(expr1) match {
        case Some((dot1, ty1)) if ty1 == t =>
          computeType(expr2) match {
            case Some((dot2, ty2)) if ty2 == t => Some((Math.max (dot1, dot2), r))
            case _ => None
          }
        case _ => None
      }
    }

    def computeType(expr : E) : Option[(Int, Domain.T)] = {
      expr match {
        case E.VIN(index) => 
          if (index == 0) {
            grammar.getOpt(lhs).map (n => (0, n.inT))
          } else if (index > 0 && index <= rule.rhs.length) {
            rule.rhs(index - 1)._1 match {
              case n : NS => grammar.getOpt(n).map(n => (index, n.inT))
              case t : TS => grammar.getOpt(t).map(t => (index, t.inT))
            }
          } else None
        case E.VOUT(index) => 
          if (index > 0 && index <= rule.rhs.length) {
            rule.rhs(index - 1)._1 match {
              case n : NS => grammar.getOpt(n).map(n => (index, n.outT))
              case t : TS => grammar.getOpt(t).map(t => (index, t.outT))
            }            
          } else None
        case _ : E.INT => Some((0, T.INT))
        case _ : E.BOOL => Some((0, T.BOOL))
        case E.TUPLE(args) => typeTuple(args)
        case E.NTH(expr, n) => 
          computeType(expr) match {
            case Some((dot, T.TUPLE(types))) if n >= 0 && n < types.length => Some((dot, types(n)))
            case _ => None
          }
        case E.EQ(expr1, expr2) => typeBinary(expr1, expr2, T.INT, T.BOOL)
        case E.LESS(expr1, expr2) => typeBinary(expr1, expr2, T.INT, T.BOOL)
        case E.LEQ(expr1, expr2) => typeBinary(expr1, expr2, T.INT, T.BOOL)
        case E.NOT(expr) => typeUnary(expr, T.BOOL, T.BOOL)
        case E.OR(expr1, expr2) => typeBinary(expr1, expr2, T.BOOL, T.BOOL)
        case E.AND(expr1, expr2) => typeBinary(expr1, expr2, T.BOOL, T.BOOL)
        case E.IF(cond, expr1, expr2) => 
          (computeType(cond), computeType(expr1), computeType(expr2)) match {
            case (Some((dot, T.BOOL)), Some((dot1, ty1)), Some((dot2, ty2))) if ty1 == ty2 => 
              Some((Math.max (dot, Math.max(dot1, dot2))), ty1)
            case _ => None
          }
        case E.ADD(expr1, expr2) => typeBinary(expr1, expr2, T.INT, T.INT)
        case E.MINUS(expr1, expr2) => typeBinary(expr1, expr2, T.INT, T.INT)
        case E.MIN(expr1, expr2) => typeBinary(expr1, expr2, T.INT, T.INT)
        case E.MAX(expr1, expr2) => typeBinary(expr1, expr2, T.INT, T.INT)
      }
    }

    computeType(expr)

  }

  /** Checks if a grammar is wellformed. */
  def check[CHAR](g : Grammar[CHAR])  {

    case class Err(s : String) extends RuntimeException

    def fail[T](s : String) {
      throw Err("error checking grammar: " + s)
    }
    if (g.nonterminals.size == 0) fail("start nonterminal expected")
    val startsymbol = g.nonterminals(0)
    if (startsymbol.inT != Domain.T.UNIT || startsymbol.outT != Domain.T.UNIT) 
      fail("start nonterminal must have unit input and output types")

    def checkRule(lhs : NS, n : Nonterminal, rule : Rule) {
      // check rule.out
      typeExpr(g, lhs, rule, rule.out) match {
        case None => fail("invalid result of rule")
        case Some((dot, ty)) => 
          if (ty != n.outT) fail("result of rule has wrong type:" + ty +", expected type: " + n.outT)
      }

      // check rule.guard
      typeExpr(g, lhs, rule, rule.guard) match {
        case None => fail("invalid guard")
        case Some((dot, guardTy)) => 
          if (guardTy != Domain.T.BOOL) fail("wrong guard type: " + guardTy)
          if (dot > 0) fail("guard has invalid dependencies")
      }

      // check rule.rhs
      var dot = 0
      for ((symbol, inExpr) <- rule.rhs) {
        val ty = 
          symbol match {
            case symbol : NS => 
              g.getOpt(symbol) match {
                case Some(nonterminal) => nonterminal.inT
                case None => fail("unknown nonterminal " + symbol)
              }
            case symbol : TS => 
              g.getOpt(symbol) match {
                case Some(terminal) => terminal.inT
                case None => fail("unknown terminal " + symbol)
              }
          }
        typeExpr(g, lhs, rule, inExpr) match {
          case Some((minDot, inExprTy)) =>
            if (inExprTy != ty) fail ("symbol at position " + (dot + 1) + " has wrong type: " + inExprTy + ", expected type: " + ty)
            if (minDot > dot) fail ("symbol input at position " + (dot + 1) + " has invalid dependencies")   
          case None => fail ("symbol input at position " + (dot + 1) + " is invalid")       
        }
        dot += 1
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
          checkRule(ns, n, rule)
        } catch {
          case Err(message) => fail("nonterminal " + index + ", rule " + ruleindex + ": " + message)
        }
        ruleindex += 1
      }
      index += 1
    }
  }


}
