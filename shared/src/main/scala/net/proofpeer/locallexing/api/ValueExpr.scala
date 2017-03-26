package net.proofpeer.locallexing.api

sealed abstract class ValueExpr extends Annotated

final object ValueExpr {

  final case class VUnit() extends ValueExpr

  final case class VThis() extends ValueExpr

  final case class VFail() extends ValueExpr

  final case class VVar(name : VarName) extends ValueExpr

  final case class VAbort() extends ValueExpr

  final case class VInteger(i: BigInt) extends ValueExpr

  final case class VString(codepoints : Vector[Int]) extends ValueExpr

  final case class VBoolean(b : Boolean) extends ValueExpr

  final case class VTuple(elems : Vector[ValueExpr]) extends ValueExpr {
    if (elems.size < 2) throw new RuntimeException("tuple must have 2 elements or more")
  }

  final case class VRecord(fields : Map[String, ValueExpr]) extends ValueExpr 

  final case class VVector(elems : Vector[ValueExpr]) extends ValueExpr

  final case class VSet(elems : Vector[ValueExpr]) extends ValueExpr

  final case class VMap(mappings : Vector[(ValueExpr, ValueExpr)]) extends ValueExpr

  final case class VIf(cond : ValueExpr, vtrue : ValueExpr, vfalse : ValueExpr) extends ValueExpr

/*  final case class VEq(x : ValueExpr, y : ValueExpr) extends ValueExpr

  final case class VLeq(x : ValueExpr, y : ValueExpr) extends ValueExpr

  final case class VLess(x : ValueExpr, y : ValueExpr) extends ValueExpr*/

}