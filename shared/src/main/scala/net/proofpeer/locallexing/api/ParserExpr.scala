package net.proofpeer.locallexing.api

sealed abstract class ParserExpr extends Annotated

final object ParserExpr {

  final case class Sequence(parsers : Vector[(ParserExpr, Option[VarName])], value : Option[ValueExpr]) extends ParserExpr

  final case class Choice(parser1 : ParserExpr, parser2 : ParserExpr) extends ParserExpr

  final case class Optional(parser : ParserExpr) extends ParserExpr

  final case class Repeat(parser : ParserExpr) extends ParserExpr

  final case class Repeat1(parser : ParserExpr) extends ParserExpr

  // this represents either a nonterminal or a terminal, depending on what 'name' points to
  final case class Call(name : Name, param : ValueExpr) extends ParserExpr

  final case class Lexer(lexer : LexerExpr) extends ParserExpr

  def choice(parsers : ParserExpr*) : ParserExpr = {
    if (parsers.isEmpty) Lexer(LexerExpr.Fail())
    else if (parsers.tail.isEmpty) parsers.head
    else {
      var parser = parsers.head
      for (p <- parsers.tail) {
        parser = Choice(parser, p)
      }
      parser
    }
  }

}
