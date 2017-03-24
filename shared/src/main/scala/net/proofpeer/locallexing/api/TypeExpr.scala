package net.proofpeer.locallexing.api

sealed trait TypeExpr

final object TypeExpr {

  final case object TAny extends TypeExpr

  final case object TNone extends TypeExpr

  final case object TUnit extends TypeExpr

  final case object TInteger extends TypeExpr

  final case object TBoolean extends TypeExpr

  final case object TString extends TypeExpr

  final case class TTuple(elems : Vector[TypeExpr]) extends TypeExpr {
    if (elems.size < 2) throw new RuntimeException("tuple must have 2 elements or more")
  }

  final case class TRecord(elems : Map[String, TypeExpr]) extends TypeExpr

  final case class TVector(elem : TypeExpr) extends TypeExpr

  final case class TCustom(name : Name) extends TypeExpr

  type TypeEnv = Name => TypeExpr

  def join (typeEnv : TypeEnv, type1 : TypeExpr, type2 : TypeExpr) : TypeExpr = {
    def j(ty1 : TypeExpr, ty2 : TypeExpr) : TypeExpr = {
      (ty1, ty2) match {
        case (TNone, t) => t
        case (t, TNone) => t
        case (TAny, _) => TAny
        case (_, TAny) => TAny
        case (TUnit, TUnit) => TUnit
        case (TInteger, TInteger) => TInteger
        case (TBoolean, TBoolean) => TBoolean
        case (TString, TString) => TString
        case (TTuple(elems1), TTuple(elems2)) =>
          if (elems1.size == elems2.size) {
            TTuple((0 until elems1.size).map(i => j(elems1(i), elems2(i))).toVector)
          } else TAny
        case (TRecord(elems1), TRecord(elems2)) =>
          val keys = elems1.keys
          if (keys == elems2.keys) {
            TRecord(keys.map(k => (k, j(elems1(k), elems2(k)))).toMap)
          } else TAny
        case (TVector(elem1), TVector(elem2)) => TVector(j(elem1, elem2))
        case (TCustom(name1), TCustom(name2)) => 
          if (name1 == name2) type1 else j(typeEnv(name1), typeEnv(name2))
        case (TCustom(name), t) => j(typeEnv(name), t)
        case (t, TCustom(name)) => j(t, typeEnv(name))
        case _ => TAny
      }
    }
    j(type1, type2)
  }

  def typesAreEqual(typeEnv : TypeEnv, ty1 : TypeExpr, ty2 : TypeExpr) : Boolean = {
    def cmp(ty1 : TypeExpr, ty2 : TypeExpr) : Boolean = {
      (ty1, ty2) match {
        case (TNone, TNone) => true
        case (TAny, TAny) => true
        case (TUnit, TUnit) => true
        case (TInteger, TInteger) => true
        case (TBoolean, TBoolean) => true
        case (TString, TString) => true
        case (TTuple(elems1), TTuple(elems2)) =>
          if (elems1.size == elems2.size) {
            (0 until elems1.size).forall(i => cmp(elems1(i), elems2(i)))
          } else false
        case (TRecord(elems1), TRecord(elems2)) =>
          val keys = elems1.keys
          if (keys == elems2.keys) {
            keys.forall(k => cmp(elems1(k), elems2(k)))
          } else false
        case (TVector(elem1), TVector(elem2)) => 
          cmp(elem1, elem2)
        case (TCustom(name1), TCustom(name2)) => 
          if (name1 == name2) true
          else cmp(typeEnv(name1), typeEnv(name2))
        case (TCustom(name), t) => cmp(typeEnv(name), t)
        case (t, TCustom(name)) => cmp(t, typeEnv(name))
        case _ => false
      }
    }
    cmp(ty1, ty2)
  }

  def isSubtype(typeEnv : TypeEnv, subty : TypeExpr, ty : TypeExpr) : Boolean = {
    typesAreEqual(typeEnv, join(typeEnv, subty, ty), ty)
  }

  case class TypingError(error : String) extends RuntimeException

  def typeValueExpr(typeEnv : TypeEnv, env : Env, value : ValueExpr) : TypeExpr = {
    def typeit(value : ValueExpr) : TypeExpr = {
      value match {
        case ValueExpr.VUnit => TUnit
        case ValueExpr.VThis => env._1
        case ValueExpr.VVar(varname) => 
          env._2.get(varname) match {
            case None => throw TypingError("invalid reference to '" + varname + "'")
            case Some(t) => t
          }
        case _ : ValueExpr.VInteger => TInteger
        case _ : ValueExpr.VBoolean => TBoolean
        case _ : ValueExpr.VString => TString
      }
    }
    typeit(value)
  }

  def typeLexerExpr(typeEnv : TypeEnv, env : Env, lexer : LexerExpr) : TypeExpr = {
    def typek(obj : String, lexer : LexerExpr, k : ValueExpr) : TypeExpr = {
      val ty = typeit(env, lexer)
      val kty = typeValueExpr(typeEnv, env, k)
      if (!typesAreEqual(typeEnv, kty, TInteger)) 
        throw TypingError(obj + " value must be an integer")
      ty

    }
    def typeit(env : Env, lexer : LexerExpr) : TypeExpr = {
      lexer match {
        case LexerExpr.Fail => TNone
        case LexerExpr.Empty => TUnit
        case LexerExpr.Character(min, max) => 
          val minTy = typeValueExpr(typeEnv, env, min)
          val maxTy = typeValueExpr(typeEnv, env, max)
          if (!typesAreEqual(typeEnv, minTy, TInteger) && !typesAreEqual(typeEnv, minTy, TString)) {
            throw TypingError("min character bound must be a string or an integer")
          }
          if (!typesAreEqual(typeEnv, maxTy, TInteger) && !typesAreEqual(typeEnv, maxTy, TString)) {
            throw TypingError("max character bound must be a string or an integer")
          }
          TUnit
        case LexerExpr.Choice(lexer1, lexer2) =>
          join(typeEnv, typeit(env, lexer1), typeit(env, lexer2))
        case LexerExpr.Sequence(lexers, value) =>
          var currentEnv = env
          var types : Vector[TypeExpr] = Vector()
          var none : Boolean = false
          for ((lexer, binder) <- lexers) {
            val ty = typeit(currentEnv, lexer)
            if (typesAreEqual(typeEnv, ty, TNone)) none = true
            if (!none && !typesAreEqual(typeEnv, ty, TUnit)) types = types :+ ty
            binder match {
              case None => // do nothing
              case Some(varname) => currentEnv = updateEnv(currentEnv, varname, ty)
            }
          }
          value match {
            case None => 
              if (none) TNone 
              else if (types.isEmpty) TUnit
              else if (types.size == 1) types(0)
              else TTuple(types)
            case Some(value) =>
              val ty = typeValueExpr(typeEnv, currentEnv, value)
              if (none) TNone
              else ty
          }
        case LexerExpr.Optional(lexer) => TVector(typeit(env, lexer))
        case LexerExpr.Repeat(lexer) => TVector(typeit(env, lexer))
        case LexerExpr.Repeat1(lexer) => TVector(typeit(env, lexer))
        case LexerExpr.And(lexer) => typeit(env, lexer)
        case LexerExpr.Not(lexer) => 
          val _ = typeit(env, lexer)
          TUnit
        case LexerExpr.ReverseAnd(lexer) => typeit(env, lexer)
        case LexerExpr.ReverseNot(lexer) =>
          val _ = typeit(env, lexer)
          TUnit
        case LexerExpr.Word(lexer) => typeit(env, lexer)
        case LexerExpr.Line(lexer) => typeit(env, lexer)
        case LexerExpr.Paragraph(lexer) => typeit(env, lexer)
        case LexerExpr.ColumnGeq(lexer, k) => typek("column", lexer, k)
        case LexerExpr.ColumnLeq(lexer, k) => typek("column", lexer, k)
        case LexerExpr.RowGeq(lexer, k) => typek("row", lexer, k)
        case LexerExpr.RowLeq(lexer, k) => typek("row", lexer, k)
        case LexerExpr.Call(name, param) => 
          env._3.get(name) match {
            case None => throw TypingError("unknown call target '" + name + "'")
            case Some((src, dest)) =>
              val ty = typeValueExpr(typeEnv, env, param)
              if (isSubtype(typeEnv, ty, src)) dest
              else throw TypingError("illegal argument to '" + name +"'")  
          }
      }
    }
    typeit(env, lexer)
  }

}



