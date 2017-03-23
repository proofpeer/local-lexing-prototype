package net.proofpeer.locallexing

import net.proofpeer.locallexing.kernel._

package object layout {

  /** (row, column, characterCode) */
  type LaidoutChar = (Int, Int, Int) 

  type LaidoutInput = Input[LaidoutChar]

  final case class Geometry(firstcol : Int, firstrow : Int, lastcol : Int, lastrow : Int, leftmost : Int) {

    def add(g : Geometry) : Geometry = {
      if (firstcol < 0) return g
      if (g.firstcol < 0) return this
      var (fc, fr) = 
        if (Geometry.before(firstcol, firstrow, g.firstcol, g.firstrow)) 
          (firstcol, firstrow)
        else
          (g.firstcol, g.firstrow)
      var (lc, lr) = 
        if (Geometry.before(lastcol, lastrow, g.lastcol, g.lastrow)) 
          (g.lastcol, g.lastrow)
        else
          (lastcol, lastrow)
      Geometry(fc, fr, lc, lr, Math.min(leftmost, g.leftmost))
    }     

  }

  final object Geometry {

    val empty = Geometry(-1, -1, -1, -1, -1)

    def singleton(col : Int, row : Int) : Geometry = Geometry(col, row, col, row, col)

    def before(col1 : Int, row1 : Int, col2 : Int, row2 : Int) : Boolean = {
      row1 < row2 || (row1 == row2 && col1 < col2)
    }

  }

}
