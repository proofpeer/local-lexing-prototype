package com.locallexing.kernel

final object Domain {

  sealed trait T
  final object T {
    final case object INT extends T
    final case object BOOL extends T
    final case class TUPLE (args : Vector[T]) extends T
    final val UNIT = TUPLE(Vector())
  }

  sealed trait V
  final object V {
    final case class INT(value : Int) extends V
    final case object UNDEFINED_INT extends V
    final case class BOOL(value : Boolean) extends V
    final case object UNDEFINED_BOOL extends V
    final case class TUPLE(args : Vector[V]) extends V
    final val NIL = TUPLE(Vector())
  }

  trait Environment {
    def inputAt(index : Int) : Domain.V
    def outputAt(index : Int) : Domain.V
  }

  sealed trait Expr 
  final object Expr {
    /** The nonterminal on the left hand side of a rule has index 0, the right hand side symbols start at index 1 */
    final case class VIN(index : Int) extends Expr
    final case class VOUT(index : Int) extends Expr
    
    final case class INT(value : Int) extends Expr 
    final case class BOOL(value : Boolean) extends Expr
    final case class TUPLE(args : Vector[Expr]) extends Expr
    final case class NTH(expr : Expr, n : Int) extends Expr /* n >= 1 */
    final case class EQ(expr1 : Expr, expr2 : Expr) extends Expr
    final case class LESS(expr1 : Expr, expr2 : Expr) extends Expr
    final case class LEQ(expr1 : Expr, expr2 : Expr) extends Expr
    final case class NOT(expr : Expr) extends Expr
    final case class OR(expr1 : Expr, expr2 : Expr) extends Expr
    final case class AND(expr1 : Expr, expr2 : Expr) extends Expr
    final case class IF(cond : Expr, expr1 : Expr, expr2 : Expr) extends Expr
    final case class ADD(expr1 : Expr, expr2 : Expr) extends Expr
    final case class MINUS(expr1 : Expr, expr2 : Expr) extends Expr
    final case class MIN(expr1 : Expr, expr2 : Expr) extends Expr
    final case class MAX(expr1 : Expr, expr2 : Expr) extends Expr

    final val NIL = TUPLE(Vector())

    def intEval(env : Environment, expr : Expr) : Int = eval(env, expr).asInstanceOf[V.INT].value

    def boolEval(env : Environment, expr : Expr) : Boolean = eval(env, expr).asInstanceOf[V.BOOL].value

    def eval(env : Environment, expr : Expr) : Domain.V = {
      expr match {
        case NIL => V.NIL
        case VIN(index) => env.inputAt(index)
        case VOUT(index) => env.outputAt(index)
        case INT(value) => V.INT(value)
        case BOOL(value) => V.BOOL(value)
        case TUPLE(args) => V.TUPLE(args.map(e => eval(env, e)))
        case NTH(tuple, n) => eval(env, tuple).asInstanceOf[V.TUPLE].args(n - 1)
        case EQ(expr1, expr2) => V.BOOL(eval(env, expr1) == eval(env, expr2))
        case LESS(expr1, expr2) => V.BOOL(intEval(env, expr1) < intEval(env, expr2))
        case LEQ(expr1, expr2) => V.BOOL(intEval(env, expr1) <= intEval(env, expr2))
        case NOT(expr) => V.BOOL(!boolEval(env, expr))
        case OR(expr1, expr2) => V.BOOL(boolEval(env, expr1) || boolEval(env, expr2))
        case AND(expr1, expr2) => V.BOOL(boolEval(env, expr1) && boolEval(env, expr2))
        case IF(cond, expr1, expr2) => if (boolEval(env, cond)) eval(env, expr1) else eval(env, expr2)
        case ADD(expr1, expr2) => V.INT(intEval(env, expr1) + intEval(env, expr2))
        case MINUS(expr1, expr2) => V.INT(intEval(env, expr1) - intEval(env, expr2))
        case MIN(expr1, expr2) => V.INT(Math.min(intEval(env, expr1), intEval(env, expr2)))
        case MAX(expr1, expr2) => V.INT(Math.max(intEval(env, expr1), intEval(env, expr2)))        
      }
    }

  }

}