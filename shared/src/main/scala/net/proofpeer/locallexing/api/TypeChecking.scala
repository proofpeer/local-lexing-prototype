package net.proofpeer.locallexing.api

trait TypeEnv {

  // maps a custom type name to its defining type 
  def lookup(er : ErrorRecorder, typename : Name) : TypeExpr

}

final object TypeChecking {

  case class UnknownVVar(v : ValueExpr.VVar) 
    extends ErrorMessage("unknown variable %0", Vector(v))

  case class UnknownName(name : Name)
    extends ErrorMessage("unknown name %0", Vector(name))

  case class ValueHasWrongType(v : ValueExpr, ty : TypeExpr, expected_ty : TypeExpr)
    extends ErrorMessage("value %0 has type %1 instead of expected type %2", Vector(v, ty, expected_ty))

}

final class TypeChecking(typeEnv : TypeEnv, er : ErrorRecorder) {

  import TypeExpr._
  import TypeChecking._

  def typesAreEqual(ty1 : TypeExpr, ty2 : TypeExpr) : Boolean = {
    (ty1, ty2) match {
      case (TNone(), TNone()) => true
      case (TAny(), TAny()) => true
      case (TInteger(), TInteger()) => true
      case (TBoolean(), TBoolean()) => true
      case (TString(), TString()) => true
      case (TTuple(elems1), TTuple(elems2)) =>
        if (elems1.size == elems2.size) {
          (0 until elems1.size).forall(i => typesAreEqual(elems1(i), elems2(i)))
        } else false
      case (TRecord(fields1), TRecord(fields2)) =>
        val keys = fields1.keys
        if (keys == fields2.keys) {
          keys.forall(k => typesAreEqual(fields1(k), fields2(k)))
        } else false
      case (TVector(elem1), TVector(elem2)) => 
        typesAreEqual(elem1, elem2)
      case (TSet(elem1), TSet(elem2)) =>
        typesAreEqual(elem1, elem2)
      case (TMap(domain1, target1), TMap(domain2, target2)) =>
        typesAreEqual(domain1, domain2) && typesAreEqual(target1, target2)
      case (TCustom(name1), TCustom(name2)) => 
        if (name1 == name2) true
        else typesAreEqual(typeEnv.lookup(er, name1), typeEnv.lookup(er, name2))
      case (TCustom(name), t) => typesAreEqual(typeEnv.lookup(er, name), t)
      case (t, TCustom(name)) => typesAreEqual(t, typeEnv.lookup(er, name))
      case _ => false
    }
  }

  def join(ty1 : TypeExpr, ty2 : TypeExpr) : TypeExpr = {
    (ty1, ty2) match {
      case (_ : TNone, t) => t
      case (t, _ : TNone) => t
      case (_ : TAny, _) => tany
      case (_, _ : TAny) => tany
      case (_: TInteger, _ : TInteger) => tinteger
      case (_ : TBoolean, _ : TBoolean) => tboolean
      case (_ : TString, _ : TString) => tstring
      case (TTuple(elems1), TTuple(elems2)) =>
        if (elems1.size == elems2.size) {
          TTuple((0 until elems1.size).map(i => join(elems1(i), elems2(i))).toVector)
        } else tany
      case (TRecord(fields1), TRecord(fields2)) =>
        val keys = fields1.keys
        if (keys == fields2.keys) {
          TRecord(keys.map(k => (k, join(fields1(k), fields2(k)))).toMap)
        } else tany
      case (TVector(elem1), TVector(elem2)) => TVector(join(elem1, elem2))
      case (TSet(elem1), TSet(elem2)) => TSet(join(elem1, elem2))
      case (TMap(domain1, target1), TMap(domain2, target2)) => 
        TMap(join(domain1, domain2), join(target1, target2))
      case (TCustom(name1), TCustom(name2)) => 
        if (name1 == name2) ty1 else join(typeEnv.lookup(er, name1), typeEnv.lookup(er, name2))
      case (TCustom(name), t) => join(typeEnv.lookup(er, name), t)
      case (t, TCustom(name)) => join(t, typeEnv.lookup(er, name))
      case _ => tany
    }
  }

  def isSubtype(subty : TypeExpr, ty : TypeExpr) : Boolean = {
    typesAreEqual(join(subty, ty), ty)
  }

  def isTUnit(ty : TypeExpr) : Boolean = {
    typesAreEqual(ty, tunit)
  }

  def isTNone(ty : TypeExpr) : Boolean = {
    typesAreEqual(ty, tnone)
  }

  def typeValueExpr(env : Env, value : ValueExpr) : TypeExpr = {
    def typeit(value : ValueExpr) : TypeExpr = {
      value match {
        case ValueExpr.VUnit() => tunit
        case ValueExpr.VThis() => env._1
        case ValueExpr.VAbort() => tnone
        case ValueExpr.VFail() => tnone
        case vvar@ValueExpr.VVar(varname) => 
          env._2.get(varname) match {
            case None => 
              er.record(UnknownVVar(vvar))
              tnone
            case Some(t) => t
          }
        case _ : ValueExpr.VInteger => tinteger
        case _ : ValueExpr.VBoolean => tboolean
        case _ : ValueExpr.VString => tstring
        case ValueExpr.VTuple(elems) => TTuple(elems.map(typeit _))
        case ValueExpr.VRecord(fields) => TRecord(fields.mapValues(typeit _))
        case ValueExpr.VSet(elems) => TSet(elems.foldLeft(tnone){case (t, v) => join(t, typeit(v))})
        case ValueExpr.VVector(elems) => TVector(elems.foldLeft(tnone){case (t, v) => join(t, typeit(v))})         
        case ValueExpr.VMap(mappings) =>
          var domain = tnone
          var target = tnone
          for ((d, t) <- mappings) {
            domain = join(domain, typeit(d))
            target = join(target, typeit(t))
          }
          TMap(domain, target)
        case ValueExpr.VIf(cond, vtrue, vfalse) =>
          val result_ty = join(typeit(vtrue), typeit(vfalse))
          val sig = Signature(Signature(tboolean)(result_ty))
          Signature.check(sig, env, Vector(cond))
      }
    }
    typeit(value)
  }

  final object Signature {

    import TypeExpr._

    type Instance = (Vector[TypeExpr], TypeExpr)
    type Signature = Vector[Instance]

    def apply(args : TypeExpr*)(result : TypeExpr) : Instance = {
      (args.toVector, result)
    }

    def apply(instances : Instance*) : Signature = {
      instances.toVector
    }

    private def checkInstance(instance : Instance, env : Env, args : Vector[ValueExpr]) : 
      Either[TypeExpr, Vector[ErrorMessage]] = 
    {
      if (instance._1.size != args.size) throw new RuntimeException("Signature.check: internal error")
      val types = args.map(v => typeValueExpr(env, v))
      var errors : Vector[ErrorMessage] = Vector()
      for (i <- 0 until types.size) {
        if (!isSubtype(types(i), instance._1(i))) 
          errors = errors :+ ValueHasWrongType(args(i), types(i), instance._1(i))
      }
      if (errors.isEmpty)
        Left(instance._2)
      else
        Right(errors)
    }

    def check(signature : Signature, env : Env, args : Vector[ValueExpr]) : TypeExpr = {
      var errors : Vector[ErrorMessage] = null
      for (instance <- signature) {
        checkInstance(instance, env, args) match {
          case Left(ty) => return ty
          case Right(errs) => 
            if (errors == null) errors = errs
            else if (errs.size < errors.size) errors = errs
        }
      }
      for (error <- errors) er.record(error)
      tnone
    }

  }  

  final val sigCharacter =
    Signature(
      Signature(tinteger, tinteger)(tunit), 
      Signature(tstring, tstring)(tunit), 
      Signature(tinteger, tstring)(tunit),
      Signature(tstring, tinteger)(tunit))
 
  def typeLexerExpr(env : Env, lexer : LexerExpr) : TypeExpr = {
    def typek(lexer : LexerExpr, k : ValueExpr) : TypeExpr = {
      val lexer_ty = typeit(env, lexer)
      val sig = Signature(Signature(tinteger)(lexer_ty))
      Signature.check(sig, env, Vector(k))
    }
    def typeit(env : Env, lexer : LexerExpr) : TypeExpr = {
      lexer match {
        case LexerExpr.Fail() => tnone
        case LexerExpr.Empty() => tunit
        case LexerExpr.Character(min, max) => Signature.check(sigCharacter, env, Vector(min, max))
        case LexerExpr.Choice(lexer1, lexer2) => join(typeit(env, lexer1), typeit(env, lexer2))
        case LexerExpr.Sequence(lexers, value) =>
          var currentEnv = env
          var types : Vector[TypeExpr] = Vector()
          for ((lexer, binder) <- lexers) {
            val ty = typeit(currentEnv, lexer)
            if (!isTUnit(ty)) types = types :+ ty
            binder match {
              case None => // do nothing
              case Some(varname) => currentEnv = updateEnv(currentEnv, varname, ty)
            }
          }
          value match {
            case None => 
              if (types.isEmpty) tunit
              else if (types.size == 1) types(0)
              else TTuple(types)
            case Some(value) =>
              typeValueExpr(currentEnv, value)
          }
        case LexerExpr.Optional(lexer) => TVector(typeit(env, lexer))
        case LexerExpr.Repeat(lexer) => TVector(typeit(env, lexer))
        case LexerExpr.Repeat1(lexer) => TVector(typeit(env, lexer))
        case LexerExpr.And(lexer) => typeit(env, lexer)
        case LexerExpr.Not(lexer) => 
          val _ = typeit(env, lexer)
          tunit
        case LexerExpr.ReverseAnd(lexer) => typeit(env, lexer)
        case LexerExpr.ReverseNot(lexer) =>
          val _ = typeit(env, lexer)
          tunit
        case LexerExpr.Word(lexer) => typeit(env, lexer)
        case LexerExpr.Line(lexer) => typeit(env, lexer)
        case LexerExpr.Paragraph(lexer) => typeit(env, lexer)
        case LexerExpr.ColumnGeq(lexer, k) => typek(lexer, k)
        case LexerExpr.ColumnLeq(lexer, k) => typek(lexer, k)
        case LexerExpr.RowGeq(lexer, k) => typek(lexer, k)
        case LexerExpr.RowLeq(lexer, k) => typek(lexer, k)
        case LexerExpr.Call(name, param) => 
          env._3.get(name) match {
            case None => 
              val _ = typeValueExpr(env, param)
              er.record(UnknownName(name))
              tnone
            case Some((src, dest)) =>
              val sig = Signature(Signature(src)(dest))
              Signature.check(sig, env, Vector(param))
          } 
      }
    }
    typeit(env, lexer)
  }

}
