package net.proofpeer.locallexing.api

sealed abstract class LayoutExpr[A] extends Annotated

final object LayoutExpr {

  final case class FirstCol[A]() extends LayoutExpr[A]

  final case class FirstRow[A]() extends LayoutExpr[A]

  final case class LastCol[A]() extends LayoutExpr[A]

  final case class LastRow[A]() extends LayoutExpr[A]

  final case class LeftMost[A]() extends LayoutExpr[A]

}