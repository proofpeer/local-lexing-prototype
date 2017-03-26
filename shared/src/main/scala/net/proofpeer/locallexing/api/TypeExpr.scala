package net.proofpeer.locallexing.api

sealed abstract class TypeExpr extends Annotated

final object TypeExpr {

  final case class TAny() extends TypeExpr

  final case class TNone() extends TypeExpr

  final case class TUnit() extends TypeExpr

  final case class TInteger() extends TypeExpr

  final case class TBoolean() extends TypeExpr

  final case class TString() extends TypeExpr

  final case class TTuple(elems : Vector[TypeExpr]) extends TypeExpr {
    if (elems.size < 2) throw new RuntimeException("tuple must have 2 elements or more")
  }

  final case class TRecord(fields : Map[String, TypeExpr]) extends TypeExpr

  final case class TVector(elem : TypeExpr) extends TypeExpr

  final case class TSet(elem : TypeExpr) extends TypeExpr

  final case class TMap(domain : TypeExpr, target : TypeExpr) extends TypeExpr

  final case class TCustom(name : Name) extends TypeExpr

  private def seal(ty : TypeExpr) : TypeExpr = {
    ty.sealAnnotation(None)
    ty
  }

  val tany = seal(TAny())
  val tnone = seal(TNone())
  val tunit = seal(TUnit())
  val tinteger = seal(TInteger())
  val tboolean = seal(TBoolean())
  val tstring = seal(TString())

}



