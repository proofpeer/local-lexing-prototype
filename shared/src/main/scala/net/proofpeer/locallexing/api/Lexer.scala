package net.proofpeer.locallexing.api

import net.proofpeer.locallexing.utils.Range
import net.proofpeer.locallexing.utils.StringUtils

sealed trait Lexer

final object Lexer {

  final case object Fail extends Lexer

  final case object Empty extends Lexer

  final case class Character(range : Range) extends Lexer

  final case class Choice(lexer1 : Lexer, lexer2 : Lexer) extends Lexer

  final case class Sequence(lexer1 : Lexer, lexer2 : Lexer) extends Lexer

  final case class Optional(lexer : Lexer) extends Lexer

  final case class Repeat(lexer : Lexer) extends Lexer

  final case class Repeat1(lexer : Lexer) extends Lexer

  final case class And(lexer : Lexer) extends Lexer

  final case class Not(lexer : Lexer) extends Lexer

  final case class ReverseAnd(lexer : Lexer) extends Lexer

  final case class ReverseNot(lexer : Lexer) extends Lexer

  final case class Word(lexer : Lexer) extends Lexer

  final case class Line(lexer : Lexer) extends Lexer

  final case class Paragraph(lexer : Lexer) extends Lexer

  final case class ColumnGeq(lexer : Lexer, k : Int) extends Lexer

  final case class ColumnLeq(lexer : Lexer, k : Int) extends Lexer

  final case class RowGeq(lexer : Lexer, k : Int) extends Lexer

  final case class RowLeq(lexer : Lexer, k : Int) extends Lexer

  def range(min : String, max : String) : Character = {
    (StringUtils.codePoints(min), StringUtils.codePoints(max)) match {
      case (Vector(left), Vector(right)) => Character(Range(left, right))
      case _ => throw new RuntimeException("invalid range")
    }
  }

  def range(min : Char, max : Char) : Character = Character(Range(min, max))

  def char(c : Char) : Character = Character(Range(c))

  def string(s : String) : Lexer = {
    StringUtils.codePoints(s) match {
      case Vector() => Empty
      case Vector(c) => Character(Range(c))
      case cs => 
        var lexer : Lexer = null
        for (c <- cs) {
          if (lexer == null) 
            lexer = Character(Range(c))
          else 
            lexer = Sequence(lexer, Character(Range(c)))
        }
        Word(lexer)
    }
  }

  def seq(lexers : Lexer*) : Lexer = {
    if (lexers.isEmpty) Empty
    else if (lexers.tail.isEmpty) lexers.head
    else {
      var lexer = lexers.head
      for (l <- lexers.tail) {
        lexer = Sequence(lexer, l)
      }
      lexer
    }
  }

  def choice(lexers : Lexer*) : Lexer = {
    if (lexers.isEmpty) Fail
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
