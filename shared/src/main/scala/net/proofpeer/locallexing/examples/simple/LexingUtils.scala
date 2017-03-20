package net.proofpeer.locallexing.examples.simple

final object LexingUtils {

  import net.proofpeer.locallexing.kernel._
  import Grammar._

  type P = Unit
  type LexerP[CHAR] = Lexer[CHAR, P]
  type Result = Set[(Int, P)]

  val P = ()
  val EMPTYSET : Result = Set()

  def epsilon[CHAR] = new Lexer[CHAR, P] {
    def lex(input : Input[CHAR], startPosition : Int, param : P) : Result = {
      Set((0, P))
    }        
  }

  def singleton[CHAR](c : CHAR => Boolean) = new Lexer[CHAR, P] {
    def lex(input : Input[CHAR], startPosition : Int, param : P) : Result = {
      if (startPosition >= input.size) 
        EMPTYSET
      else if (c(input.at(startPosition))) 
        Set((1, P)) 
      else 
        EMPTYSET
    }    
  }

  def char(c : Char) = singleton((d : Char) => d == c)

  def letter = singleton((c : Char) => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))

  def digit = singleton((c : Char) => (c >= '0' && c <= '9'))

  def concat[CHAR](lexer1 : LexerP[CHAR], lexer2 : LexerP[CHAR]) : LexerP[CHAR] = new LexerP[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, param : P) : Result = {
      var maxlen = -1
      for ((len1, _) <- lexer1.lex(input, startPosition, P)) {
        for ((len2, _) <- lexer2.lex(input, startPosition + len1, P))
          maxlen = Math.max(maxlen, len1 + len2)
      }
      if (maxlen < 0) EMPTYSET else Set((maxlen, P))
    }    
  } 

  def repeat[CHAR](lexer : LexerP[CHAR]) : LexerP[CHAR] = new Lexer[CHAR, P] {
    def lex(input : Input[CHAR], startPosition : Int, param : P) : Result = {
      var done : Set[Int] = Set()
      var toDo : Set[Int] = Set(0)
      while (!toDo.isEmpty) {
        for (len1 <- toDo) {
          toDo = toDo - len1
          for ((len2, _) <- lexer.lex(input, startPosition + len1, P)) {
            val len = len1 + len2
            if (!done.contains(len)) {
              done = done + len
              toDo = toDo + len
            }
          }          
        }
      }
      if (done.isEmpty) EMPTYSET else Set((done.last, P))
    }
  } 

  def repeat1[CHAR](lexer : LexerP[CHAR]) : LexerP[CHAR] = concat(lexer, repeat(lexer))

  def or[CHAR](lexer1 : LexerP[CHAR], lexer2 : LexerP[CHAR]) : LexerP[CHAR] = new LexerP[CHAR] {
    def lex(input : Input[CHAR], startPosition : Int, param : P) : Result = {
      val result1 = lexer1.lex(input, startPosition, P)
      if (result1.isEmpty) 
        lexer2.lex(input, startPosition, P)
      else
        result1
    }        
  }

  def unbiasedSelector[CHAR] : Selector[CHAR, P] = new Selector[CHAR, P] {
    def select(input : Input[CHAR], startPosition : Int, A : Tokens[P], B : Tokens[P]) = B
  }

 /* def orderSelector[CHAR](less : (TS, Int, TS, Int) => Boolean) : Selector[CHAR, P] = new Selector[CHAR, P] {

    def select(input : Input[CHAR], startPosition : Int, A : Tokens[P], B : Tokens[P]) = {
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

  }*/


} 