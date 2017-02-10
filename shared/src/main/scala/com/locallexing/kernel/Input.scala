package com.locallexing.kernel

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