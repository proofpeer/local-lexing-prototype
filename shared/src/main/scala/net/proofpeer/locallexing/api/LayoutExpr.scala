package net.proofpeer.locallexing.api

sealed trait LayoutExpr extends Annotated

final object LayoutExpr {

  final case class FirstCol() extends LayoutExpr

  final case class FirstRow() extends LayoutExpr

  final case class LastCol() extends LayoutExpr

  final case class LastRow() extends LayoutExpr

  final case class LeftMost() extends LayoutExpr

}