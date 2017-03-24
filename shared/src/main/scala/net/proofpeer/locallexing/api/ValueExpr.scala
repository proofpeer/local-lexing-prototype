package net.proofpeer.locallexing.api

sealed trait ValueExpr

final object ValueExpr {

  final case object VUnit extends ValueExpr

  final case object VThis extends ValueExpr

  final case object VFail extends ValueExpr

  final case class VVar(name : VarName) extends ValueExpr

  final case object VAbort extends ValueExpr

  final case class VInteger(i: BigInt) extends ValueExpr

  final case class VString(codepoints : Vector[Int]) extends ValueExpr

  final case class VBoolean(b : Boolean) extends ValueExpr

  final case class VTuple(elems : Vector[ValueExpr]) extends ValueExpr {
    if (elems.size < 2) throw new RuntimeException("tuple must have 2 elements or more")
  }

  final case class VRecord(fields : Map[String, ValueExpr]) extends ValueExpr 

  final case class VSet(elems : Vector[ValueExpr]) extends ValueExpr

  final case class VMap(mappings : Vector[(ValueExpr, ValueExpr)]) extends ValueExpr


}