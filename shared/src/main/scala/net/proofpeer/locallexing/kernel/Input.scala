package net.proofpeer.locallexing.kernel

trait Input[Character] {

  def size : Int

  def at(pos : Int) : Character

  def print(pos : Int, len : Int) : String

}

final class StringInput(s : String) extends Input[Char] {

  def size = s.size

  def at(pos : Int) : Char = s(pos)

  def print(pos : Int, len : Int) : String = s.substring(pos, pos + len)

}

final class VectorInput[Character](v : Vector[Character]) extends Input[Character] {

  def size = v.size

  def at(pos : Int) : Character = v(pos)

  def print(pos : Int, len : Int) : String = {
    var s = "["
    for (i <- 0 until len) {
      if (i > 0) s = s + ","
      s = s + at(pos + i).toString
    }
    s + "]"
  }

}