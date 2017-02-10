package com.locallexing.kernel

final object LexingUtils {

  import Grammar._

  private val NIL = Domain.V.NIL

  def epsilon[CHAR] = new Lexer[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, value : Domain.V) : Option[(Int, Domain.V)] = {
      Some((0, NIL))
    }        
  }

  def singleton[CHAR](c : CHAR => Boolean) = new Lexer[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, value : Domain.V) : Option[(Int, Domain.V)] = {
      if (startPosition >= input.size) 
        None
      else if (c(input.at(startPosition))) 
        Some((1, NIL)) 
      else 
        None
    }    
  }

  def char(c : Char) = singleton((d : Char) => d == c)

  def letter = singleton((c : Char) => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))

  def digit = singleton((c : Char) => (c >= '0' && c <= '9'))

  def concat[CHAR](lexer1 : Lexer[CHAR], lexer2 : Lexer[CHAR]) : Lexer[CHAR] = new Lexer[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, value : Domain.V) : Option[(Int, Domain.V)] = {
      lexer1.lex(input, startPosition, NIL) match {
        case None => None
        case Some((len1, _)) => 
          lexer2.lex(input, startPosition + len1, NIL) match {
            case None => None
            case Some((len2, _)) => Some((len1 + len2, NIL))
          }
      }
    }    
  } 

  def repeat[CHAR](lexer : Lexer[CHAR]) : Lexer[CHAR] = new Lexer[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, value : Domain.V) : Option[(Int, Domain.V)] = {
      var len = 0
      do {
        lexer.lex(input, startPosition + len, NIL) match {
          case None => return Some((len, NIL))
          case Some((l, _)) => 
            if (l > 0) len += l else return Some((len, NIL))
        }
      } while (true)
      None
    }
  } 

  def repeat1[CHAR](lexer : Lexer[CHAR]) : Lexer[CHAR] = concat(lexer, repeat(lexer))

  def or[CHAR](lexer1 : Lexer[CHAR], lexer2 : Lexer[CHAR]) : Lexer[CHAR] = new Lexer[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, value : Domain.V) : Option[(Int, Domain.V)] = {
      lexer1.lex(input, startPosition, NIL) match {
        case None => 
          lexer2.lex(input, startPosition, NIL) match {
            case None => None
            case Some((len2, _)) => Some((len2, NIL))
          }
        case Some((len1, _)) => Some((len1, NIL))
      }
    }        
  }

  def unbiasedSelector[CHAR] : Selector[CHAR] = new Selector[CHAR] {
    def select(input : Input[CHAR], startPosition : Int, A : Tokens, B : Tokens) = B
  }

  def orderSelector[CHAR](less : (TS, Int, TS, Int) => Boolean) : Selector[CHAR] = new Selector[CHAR] {

    def select(input : Input[CHAR], startPosition : Int, A : Tokens, B : Tokens) = {
      var tokens = A
      for (y <- B) {
        var is_max = true
        for (x <- B) {
          if (less(y._1._1, y._2._1, x._1._1, x._2._1)) {
            is_max = false
          }
        }
        if (is_max) tokens = tokens + y
      }
      tokens
    }

  }

  def conservativeOrderSelector[CHAR](less : (TS, Int, TS, Int) => Boolean) : Selector[CHAR] = new Selector[CHAR] {

    def select(input : Input[CHAR], startPosition : Int, A : Tokens, B : Tokens) = {
      var C : Tokens = Map()
      for (y <- B) {
        var shadows_max_in_A = false
        for (x <- A) {
          if (less(x._1._1, x._2._1, y._1._1, y._2._1)) shadows_max_in_A = true
        }
        if (!shadows_max_in_A) C = C + y
      }
      var tokens = A
      for (y <- C) {
        var is_max = true
        for (x <- C) {
          if (less(y._1._1, y._2._1, x._1._1, x._2._1)) is_max = false
        }
        if (is_max) tokens = tokens + y
      }
      tokens
    }

  }


} 

