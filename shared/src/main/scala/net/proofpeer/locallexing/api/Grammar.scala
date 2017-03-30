package net.proofpeer.locallexing.api

sealed trait GrammarElem extends Annotated

final case class Grammar(elems : Vector[GrammarElem]) extends Annotated

final object Grammar {

  case class ImportSymbols(names : Vector[(LocalName, Option[LocalName])], source : Namespace) extends GrammarElem

  case class ImportTypes(names : Vector[(LocalName, Option[LocalName])], source : Namespace) extends GrammarElem

  case class TypeDef(name : LocalName, ty : TypeExpr) extends GrammarElem

  case class LexerDecl(name : LocalName, isPublic : Boolean, funty : FunType, isErrorTerminal : Boolean) extends GrammarElem

  case class LexerRule(name : LocalName, lexer : LexerExpr) extends GrammarElem

  case class ParserDecl(name : LocalName, isPublic : Boolean, funty : FunType) extends GrammarElem

  case class ParserRule(name : LocalName, parser : ParserExpr) extends GrammarElem

  case class PriorityDecl(lesser : Name, greater : Name) extends GrammarElem

}