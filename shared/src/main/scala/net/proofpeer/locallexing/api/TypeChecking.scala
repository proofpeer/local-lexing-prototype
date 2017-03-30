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

  case class InvalidFieldAccess(v : ValueExpr, ty : TypeExpr, field : FieldName)
    extends ErrorMessage("value %0 of type %1 does not have field %2", Vector(v, ty, field))

  case class InvalidTupleAccess(v : ValueExpr, ty : TypeExpr, n : TupleIndex)
    extends ErrorMessage("value %0 of type %1 cannot be accessed with tuple index %2", 
      Vector(v, ty, n))

  case class InvalidLayout(v : VarName, layoutExpr : LayoutExpr) 
    extends ErrorMessage("variable %0 does not have layout field %1", Vector(v, layoutExpr))

}

final class TypeChecking(val typeEnv : TypeEnv, val er : ErrorRecorder) {

  val Signature = new Signature(this)
  val SignatureDefs = new SignatureDefs(Signature)

  import SignatureDefs._
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

  def meet(ty1 : TypeExpr, ty2 : TypeExpr) : TypeExpr = {
    (ty1, ty2) match {
      case (_ : TNone, t) => tnone
      case (t, _ : TNone) => tnone
      case (_ : TAny, t) => t
      case (t, _ : TAny) => t
      case (_: TInteger, _ : TInteger) => tinteger
      case (_ : TBoolean, _ : TBoolean) => tboolean
      case (_ : TString, _ : TString) => tstring
      case (TTuple(elems1), TTuple(elems2)) =>
        if (elems1.size == elems2.size) {
          TTuple((0 until elems1.size).map(i => meet(elems1(i), elems2(i))).toVector)
        } else tnone
      case (TRecord(fields1), TRecord(fields2)) =>
        val keys = fields1.keys
        if (keys == fields2.keys) {
          TRecord(keys.map(k => (k, meet(fields1(k), fields2(k)))).toMap)
        } else tnone
      case (TVector(elem1), TVector(elem2)) => TVector(meet(elem1, elem2))
      case (TSet(elem1), TSet(elem2)) => TSet(meet(elem1, elem2))
      case (TMap(domain1, target1), TMap(domain2, target2)) => 
        TMap(meet(domain1, domain2), meet(target1, target2))
      case (TCustom(name1), TCustom(name2)) => 
        if (name1 == name2) ty1 else meet(typeEnv.lookup(er, name1), typeEnv.lookup(er, name2))
      case (TCustom(name), t) => meet(typeEnv.lookup(er, name), t)
      case (t, TCustom(name)) => meet(t, typeEnv.lookup(er, name))
      case _ => tnone
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

  def hasLayout(env : Env, value : ValueExpr) : Boolean = {
    value match {
      case ValueExpr.VVar(v) => env.hasLayout(v)
      case _ => false
    }
  }

  def typeValueExpr(env : Env, value : ValueExpr) : TypeExpr = {
    def typeit(value : ValueExpr) : TypeExpr = {
      value match {
        case ValueExpr.VUnit() => tunit
        case ValueExpr.VThis() => env.param
        case ValueExpr.VAbort() => tnone
        case ValueExpr.VFail() => tnone
        case vvar@ValueExpr.VVar(varname) => 
          env.vars.get(varname) match {
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
        case ValueExpr.VIf(cond, vtrue, vfalse) => Signature.check(sigIf, env, vtrue, vfalse, cond)
        case ValueExpr.VEq(x, y) => Signature.check(sigEq, env, x, y)
        case ValueExpr.VLeq(x, y) => Signature.check(sigLeq, env, x, y)
        case ValueExpr.VLess(x, y) => Signature.check(sigLess, env, x, y)
        case ValueExpr.VAnd(x, y) => Signature.check(sigAnd, env, x, y)
        case ValueExpr.VNot(x) => Signature.check(sigNot, env, x)
        case ValueExpr.VPlus(x, y) => Signature.check(sigPlus, env, x, y)
        case ValueExpr.VMinus(x, y) => Signature.check(sigMinus, env, x, y)
        case ValueExpr.VMul(x, y) => Signature.check(sigMul, env, x, y)
        case ValueExpr.VDiv(x, y) => Signature.check(sigDiv, env, x, y)
        case ValueExpr.VMod(x, y) => Signature.check(sigMod, env, x, y)
        case ValueExpr.VSize(x) => Signature.check(sigSize, env, x)
        case ValueExpr.VApply(f, x) => Signature.check(sigApply, env, f, x)
        case ValueExpr.VAccessField(record, field) => accessFieldType(value, typeit(record), field)
        case ValueExpr.VAccessTuple(tuple, n) => accessTupleElemType(value, typeit(tuple), n)
        case ValueExpr.VDispatch(value, varname, cases) =>
          val input_ty = typeit(value)
          var result_ty : TypeExpr = tnone
          val gotLayout = hasLayout(env, value)
          for ((ty, v) <- cases) {
            val case_input_ty = meet(ty, input_ty)
            val case_output_ty = typeValueExpr(updateEnv(env, varname, case_input_ty, gotLayout), v)
            result_ty = join(result_ty, case_output_ty)
          }
          result_ty
        case ValueExpr.VLet(bindings, value) =>
          var currentEnv : Env = env
          for ((v, x) <- bindings) {
            val v_ty = typeValueExpr(currentEnv, v)
            currentEnv = updateEnv(currentEnv, x, v_ty, hasLayout(currentEnv, v))
          }
          typeValueExpr(currentEnv, value)
        case ValueExpr.VLayout(varname, layoutExpr) => 
          if (env.hasLayout(varname)) tinteger
          else {
            er.record(InvalidLayout(varname, layoutExpr))
            tnone
          }
      }
    }
    typeit(value)
  }

  def meet_map_set(ty1 : TypeExpr, ty2 : TypeExpr) : TypeExpr = {
    (ty1, ty2) match {
      case (TMap(domain1, target), TSet(domain2)) => TMap(meet(domain1, domain2), target)
      case (TNone(), _) => tnone
      case (_, TNone()) => tnone
      case (TCustom(name), _) => meet_map_set(typeEnv.lookup(er, name), ty2)
      case (_, TCustom(name)) => meet_map_set(ty1, typeEnv.lookup(er, name))
      case _ => throw new RuntimeException("meet_map_set: internal error")
    }
  }

  def domainTypeOf(ty : TypeExpr) : TypeExpr = {
    ty match {
      case TMap(domain, target) => domain
      case TNone() => tnone
      case TCustom(name) => domainTypeOf(typeEnv.lookup(er, name))
      case _ => throw new RuntimeException("domainTypeOf: internal error")
    }
  }

  def targetTypeOf(ty : TypeExpr) : TypeExpr = {
    ty match {
      case TMap(domain, target) => target
      case TNone() => tnone
      case TCustom(name) => targetTypeOf(typeEnv.lookup(er, name))
      case _ => throw new RuntimeException("targetTypeOf: internal error")
    }
  }

  def accessFieldType(value : ValueExpr, ty : TypeExpr, field : FieldName) : TypeExpr = {
    ty match {
      case TRecord(fields) => 
        fields.get(field) match {
          case Some(fieldty) => fieldty
          case None => 
            er.record(InvalidFieldAccess(value, ty, field))
            tnone
        }
      case TNone() => tnone
      case TCustom(name) => accessFieldType(value, typeEnv.lookup(er, name), field)
      case _ => 
        er.record(InvalidFieldAccess(value, ty, field))
        tnone
    }
  }

  def accessTupleElemType(value : ValueExpr, ty : TypeExpr, n : TupleIndex) : TypeExpr = {
    ty match {
      case TTuple(elems) if (n.index > 0 && n.index <= elems.size) => elems(n.index)
      case TNone() => tnone
      case TCustom(name) => accessTupleElemType(value, typeEnv.lookup(er, name), n)
      case _ =>
        er.record(InvalidTupleAccess(value, ty, n))
        tnone
    }
  }

  def elemTypeOf(ty : TypeExpr) : TypeExpr = {
    ty match {
      case TSet(elem) => elem
      case TVector(elem) => elem
      case TCustom(name) => elemTypeOf(typeEnv.lookup(er, name))
      case _ => throw new RuntimeException("elemTypeOf: internal error")
    }
  }
 
  def typeLexerExpr(env : Env, lexer : LexerExpr) : TypeExpr = {
    def typek(lexer : LexerExpr, k : ValueExpr) : TypeExpr = {
      val lexer_ty = typeit(env, lexer)
      val sig = Signature(Signature.Method(lexer_ty)(tinteger))
      Signature.check(sig, env, k)
    }
    def typeit(env : Env, lexer : LexerExpr) : TypeExpr = {
      lexer match {
        case LexerExpr.Fail() => tnone
        case LexerExpr.Character(min, max) => Signature.check(sigCharacter, env, min, max)
        case LexerExpr.Choice(lexer1, lexer2) => join(typeit(env, lexer1), typeit(env, lexer2))
        case LexerExpr.Sequence(lexers, value) =>
          var currentEnv = env
          var types : Vector[TypeExpr] = Vector()
          for ((lexer, binder) <- lexers) {
            val ty = typeit(currentEnv, lexer)
            if (!isTUnit(ty)) types = types :+ ty
            binder match {
              case None => // do nothing
              case Some(varname) => currentEnv = updateEnv(currentEnv, varname, ty, true)
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
          env.functions(name) match {
            case None => 
              val _ = typeValueExpr(env, param)
              er.record(UnknownName(name))
              tnone
            case Some((src, dest)) =>
              val sig = Signature(Signature.Method(dest)(src))
              Signature.check(sig, env, param)
          } 
      }
    }
    typeit(env, lexer)
  }

  def typeParserExpr(env : Env, parser : ParserExpr) : TypeExpr = {
    def typeit(env : Env, parser : ParserExpr) : TypeExpr = {
      parser match {
        case ParserExpr.Choice(parser1, parser2) => join(typeit(env, parser1), typeit(env, parser2))
        case ParserExpr.Sequence(parsers, value) =>
          var currentEnv = env
          var types : Vector[TypeExpr] = Vector()
          for ((parser, binder) <- parsers) {
            val ty = typeit(currentEnv, parser)
            if (!isTUnit(ty)) types = types :+ ty
            binder match {
              case None => // do nothing
              case Some(varname) => currentEnv = updateEnv(currentEnv, varname, ty, true)
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
        case ParserExpr.Optional(parser) => TVector(typeit(env, parser))
        case ParserExpr.Repeat(parser) => TVector(typeit(env, parser))
        case ParserExpr.Repeat1(parser) => TVector(typeit(env, parser))
        case ParserExpr.Call(name, param) => 
          env.functions(name) match {
            case None => 
              val _ = typeValueExpr(env, param)
              er.record(UnknownName(name))
              tnone
            case Some((src, dest)) =>
              val sig = Signature(Signature.Method(dest)(src))
              Signature.check(sig, env, param)
          } 
        case ParserExpr.Lexer(lexer) => typeLexerExpr(env, lexer)
      }
    }
    typeit(env, parser)
  }  

}
