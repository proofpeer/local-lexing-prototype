package net.proofpeer.locallexing.api

sealed trait ValueExpr

final object ValueExpr {

  final case object VUnit extends ValueExpr

  final case object VThis extends ValueExpr

  final case class VVar(name : VarName) extends ValueExpr

  final case class VInteger(i: BigInt) extends ValueExpr

  final case class VString(codepoints : Vector[Int]) extends ValueExpr

  final case class VBoolean(b : Boolean) extends ValueExpr

}