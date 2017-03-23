package net.proofpeer.locallexing.layout

import net.proofpeer.locallexing.kernel._

trait LayoutLexer[P, R] {

  def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)]
 
}

final class ReverseInput[CHAR](input : Input[CHAR], startPosition : Int) extends Input[CHAR] {

  def size : Int = startPosition

  def at(pos : Int) : CHAR = input.at(startPosition - pos - 1)

  def print(pos : Int, len : Int) : String = input.print(startPosition - pos - len, len)

}

final object RegionInput {

  def neighbours(i : Int, j : Int) : Boolean = {
    i + 1 == j || j + 1 == i
  }

}

// The RegionInputs are all pretty inefficient and just for exploring the concept.

sealed abstract class RegionInput(input : Input[LaidoutChar], startPosition : Int) extends Input[LaidoutChar] {

  protected def computeRight() : Int 

  protected def computeLeft(startRow : Int, startCol : Int, startPosition : Int) : Int

  private def computeRightLeft() : (Int, Int) = {
    val input_size = input.size
    val right = computeRight()
    if (right > 0) {
      val (row, col, _) = input.at(startPosition)
      (right, computeLeft(row, col, startPosition))
    } else if (startPosition > 0) {
      val (row, col, _) = input.at(startPosition - 1)
      (right, computeLeft(row, col, startPosition - 1) + 1)
    } else (0, 0)    
  }

  val (right, left) = computeRightLeft()

  def size : Int = right + left

  def at(pos : Int) : LaidoutChar = input.at(startPosition - left + pos)

  def print(pos : Int, len : Int) : String = input.print(startPosition - left + pos, len)

}  

final class WordInput(input : Input[LaidoutChar], startPosition : Int) extends RegionInput(input, startPosition) {

  protected def computeRight() : Int = {
    val input_size = input.size
    if (startPosition >= input_size) return 0
    var (row, col, _) = input.at(startPosition)
    var right = 1
    while (startPosition + right < input_size) {
      val (r, c, _) = input.at(startPosition + right)
      if (row == r && RegionInput.neighbours(col, c)) {
        right += 1
        col = c
      } else return right
    } 
    right
  }

  protected def computeLeft(startRow : Int, startCol : Int, startPosition : Int) : Int = {
    var left = 0
    var row = startRow
    var col = startCol
    while (startPosition - left > 0) {
      val (r, c, _) = input.at(startPosition - left - 1)
      if (row == r && RegionInput.neighbours(col, c)) {
        left += 1
        col = c
      } else return left
    }  
    left
  }

}

final class LineInput(input : Input[LaidoutChar], startPosition : Int) extends RegionInput(input, startPosition) {

  protected def computeRight() : Int = {
    val input_size = input.size
    if (startPosition >= input_size) return 0
    var (row, _, _) = input.at(startPosition)
    var right = 1
    while (startPosition + right < input_size) {
      val (r, _, _) = input.at(startPosition + right)
      if (row == r) {
        right += 1
      } else return right
    } 
    right
  }

  protected def computeLeft(startRow : Int, startCol : Int, startPosition : Int) : Int = {
    var left = 0
    while (startPosition - left > 0) {
      val (r, _, _) = input.at(startPosition - left - 1)
      if (startRow == r) {
        left += 1
      } else return left
    }  
    left
  }

}

final class ParagraphInput(input : Input[LaidoutChar], startPosition : Int) extends RegionInput(input, startPosition) {

  protected def computeRight() : Int = {
    val input_size = input.size
    if (startPosition >= input_size) return 0
    var (row, _, _) = input.at(startPosition)
    var right = 1
    while (startPosition + right < input_size) {
      val (r, _, _) = input.at(startPosition + right)
      if (r == row || RegionInput.neighbours(r, row)) {
        right += 1
        row = r
      } else return right
    } 
    right
  }

  protected def computeLeft(startRow : Int, startCol : Int, startPosition : Int) : Int = {
    var left = 0
    var row = startRow
    while (startPosition - left > 0) {
      val (r, _, _) = input.at(startPosition - left - 1)
      if (r == row || RegionInput.neighbours(r, row)) {
        left += 1
        row = r
      } else return left
    }  
    left
  }

}

final class RegionPredInput(input : Input[LaidoutChar], startPosition : Int, pred : (Int, Int) => Boolean) 
  extends RegionInput(input, startPosition) 
{

  protected def computeRight() : Int = {
    val input_size = input.size
    if (startPosition >= input_size) return 0
    var (row, col, _) = input.at(startPosition)
    if (!pred(row, col)) return 0
    var right = 1
    while (startPosition + right < input_size) {
      val (r, c, _) = input.at(startPosition + right)
      if (pred(r, c)) {
        right += 1
      } else return right
    } 
    right
  }

  protected def computeLeft(startRow : Int, startCol : Int, startPosition : Int) : Int = {
    var left = 0
    while (startPosition - left > 0) {
      val (r, c, _) = input.at(startPosition - left - 1)
      if (pred(r, c)) {
        left += 1
      } else return left
    }  
    left
  }

}

final object LayoutLexer {

  def empty[P, R](transform : P => R) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        Some((0, Geometry.empty, transform(param)))
      }
    }    
  }

  def character[P](range : Range) : LayoutLexer[P, P] = {
    new LayoutLexer[P, P] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, P)] = {
        if (input.size == 0) return None
        val c = input.at(0)
        if (range.contains(c._3)) {
          Some((1, Geometry.singleton(c._2, c._1), param))
        }
        else None
      }
    }
  }

  def ordered_choice[P, R](lexer1 : LayoutLexer[P, R], lexer2 : LayoutLexer[P, R]) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        lexer1.lex(input, startPosition, param) match {
          case None => lexer2.lex(input, startPosition, param)
          case result => result
        }
      }
    }    
  }

  def sequence[P, Q, R](lexer1 : LayoutLexer[P, Q], lexer2 : LayoutLexer[Q, R]) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        lexer1.lex(input, startPosition, param) match {
          case None => None
          case Some((len1, g1, q)) => 
            lexer2.lex(input, startPosition + len1, q) match {
              case None => None
              case Some((len2, g2, r)) =>
                Some((len1 + len2, g1.add(g2), r))
            }
        }
      }
    }        
  }

  // greedy 
  def optional[P, R](lexer : LayoutLexer[P, R], none : P => R) : LayoutLexer[P, R] = 
    ordered_choice(lexer, empty(none))

  // greedy
  def one_or_more[P, R](lexer : LayoutLexer[P, R], restart : R => P) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        lexer.lex(input, startPosition, param) match {
          case None => None
          case result @ Some((len1, g1, r1)) =>
            if (len1 == 0) return result
            var r = r1
            var len = len1
            var g = g1
            while (true) {
              lexer.lex(input, startPosition + len, restart(r)) match {
                case None => return Some ((len, g, r))
                case Some((len2, g2, r2)) =>
                  len += len2
                  g = g.add(g2)
                  r = r2
                  if (len2 == 0) return Some((len, g, r))
              }
            } 
            throw new RuntimeException("internal error")
        }
      }
    }        
  }

  // greedy
  def zero_or_more[P, R](lexer : LayoutLexer[P, R], restart : R => P, zero : P => R) : LayoutLexer[P, R] = {
    ordered_choice(one_or_more(lexer, restart), empty(zero))
  }

  def and_predicate[P, R](lexer : LayoutLexer[P, R]) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        lexer.lex(input, startPosition, param) match {
          case None => None
          case Some((len, g, r)) => Some((0, Geometry.empty, r))
        }
      }
    }
  }

  def not_predicate[P, R](lexer : LayoutLexer[P, R]) : LayoutLexer[P, P] = {
    new LayoutLexer[P, P] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, P)] = {
        lexer.lex(input, startPosition, param) match {
          case None => Some((0, Geometry.empty, param))
          case Some((len, g, r)) => None
        }
      }
    }
  }

  def reverse_and_predicate[P, R](lexer : LayoutLexer[P, R]) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        lexer.lex(new ReverseInput(input, startPosition), 0, param) match {
          case None => None
          case Some((len, g, r)) => Some((0, Geometry.empty, r))
        }
      }
    }
  }

  def reverse_not_predicate[P, R](lexer : LayoutLexer[P, R]) : LayoutLexer[P, P] = {
    new LayoutLexer[P, P] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, P)] = {
        lexer.lex(new ReverseInput(input, startPosition), 0, param) match {
          case None => Some((0, Geometry.empty, param))
          case Some((len, g, r)) => None
        }
      }
    }
  }

  def word[P, R](lexer : LayoutLexer[P, R]) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        val word_input = new WordInput(input, startPosition)
        lexer.lex(word_input, word_input.left, param)
      }
    }
  }

  def line[P, R](lexer : LayoutLexer[P, R]) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        val line_input = new LineInput(input, startPosition)
        lexer.lex(line_input, line_input.left, param)
      }
    }
  }

  def paragraph[P, R](lexer : LayoutLexer[P, R]) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        val paragraph_input = new ParagraphInput(input, startPosition)
        lexer.lex(paragraph_input, paragraph_input.left, param)
      }
    }
  }

  private def region_pred[P, R](lexer : LayoutLexer[P, R], pred : (Int, Int) => Boolean) : LayoutLexer[P, R] = {
    new LayoutLexer[P, R] {
      def lex(input : LaidoutInput, startPosition : Int, param : P) : Option[(Int, Geometry, R)] = {
        val region_input = new RegionPredInput(input, startPosition, pred)
        lexer.lex(region_input, region_input.left, param)
      }
    }    
  }

  def column_ge[P,R](lexer : LayoutLexer[P, R], k : Int) = region_pred(lexer, (row, col) => col >= k)

  def column_le[P,R](lexer : LayoutLexer[P, R], k : Int) = region_pred(lexer, (row, col) => col <= k)

  def row_ge[P, R](lexer : LayoutLexer[P, R], k : Int) = region_pred(lexer, (row, col) => row >= k)

  def row_le[P, R](lexer : LayoutLexer[P, R], k : Int) = region_pred(lexer, (row, col) => row <= k)

}