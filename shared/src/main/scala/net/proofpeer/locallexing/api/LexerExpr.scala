package net.proofpeer.locallexing.api

import net.proofpeer.locallexing.utils.Range
import net.proofpeer.locallexing.utils.StringUtils

sealed abstract class LexerExpr extends Annotated 

final object LexerExpr {

  final case class Fail() extends LexerExpr

  final case class EOF() extends LexerExpr

  final case class Character(min : ValueExpr, max : ValueExpr) extends LexerExpr

  final case class Choice(lexer1 : LexerExpr, lexer2 : LexerExpr) extends LexerExpr

  final case class Sequence(lexers : Vector[(LexerExpr, Option[VarName])], value : Option[ValueExpr]) extends LexerExpr

  final case class Optional(lexer : LexerExpr) extends LexerExpr

  final case class Repeat(lexer : LexerExpr) extends LexerExpr

  final case class Repeat1(lexer : LexerExpr) extends LexerExpr

  final case class And(lexer : LexerExpr) extends LexerExpr

  final case class Not(lexer : LexerExpr) extends LexerExpr

  final case class ReverseAnd(lexer : LexerExpr) extends LexerExpr

  final case class ReverseNot(lexer : LexerExpr) extends LexerExpr

  final case class Word(lexer : LexerExpr) extends LexerExpr

  final case class Line(lexer : LexerExpr) extends LexerExpr

  final case class Paragraph(lexer : LexerExpr) extends LexerExpr

  final case class ColumnGeq(lexer : LexerExpr, k : ValueExpr) extends LexerExpr

  final case class ColumnLeq(lexer : LexerExpr, k : ValueExpr) extends LexerExpr

  final case class RowGeq(lexer : LexerExpr, k : ValueExpr) extends LexerExpr

  final case class RowLeq(lexer : LexerExpr, k : ValueExpr) extends LexerExpr

  final case class Call(name : Name, param : ValueExpr) extends LexerExpr

  def range(min : String, max : String) : Character = {
    (StringUtils.codePoints(min), StringUtils.codePoints(max)) match {
      case (Vector(left), Vector(right)) => Character(ValueExpr.VInteger(left), ValueExpr.VInteger(right))
      case _ => throw new RuntimeException("invalid range")
    }
  }

  def range(min : Int, max : Int) : Character = Character(ValueExpr.VInteger(min), ValueExpr.VInteger(max))

  def char(c : Int) : Character = range(c, c)

  def char(c : Char) : Character = range(c, c)

  def Empty() : LexerExpr = Sequence(Vector(), None)

  def string(s : String) : LexerExpr = {
    StringUtils.codePoints(s) match {
      case Vector() => Empty()
      case Vector(c) => char(c)
      case cs => Word(Sequence(cs.map(c => (char(c), None)).toVector, None))
    }
  }

  def seq(lexers : LexerExpr*) : LexerExpr = {
    if (lexers.isEmpty) Empty()
    else if (lexers.tail.isEmpty) lexers.head
    else Sequence(lexers.map(l => (l, None)).toVector, None)
  }

  def choice(lexers : LexerExpr*) : LexerExpr = {
    if (lexers.isEmpty) Fail()
    else if (lexers.tail.isEmpty) lexers.head
    else {
      var lexer = lexers.head
      for (l <- lexers.tail) {
        lexer = Choice(lexer, l)
      }
      lexer
    }
  }

}
