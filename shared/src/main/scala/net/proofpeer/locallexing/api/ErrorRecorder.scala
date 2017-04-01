package net.proofpeer.locallexing.api

import net.proofpeer.locallexing.utils.StringUtils

trait ErrorRecorder {
  def record(msg : ErrorMessage)
}

/**
 * The message can refer to its arguments via %0, %1, %2, etc. 
 */
abstract class ErrorMessage(msg : String, args : Vector[Annotated]) {
  def asString(arg : Annotated) : String = {
    val s = arg match {
      case NameSegment(s) => s
      case ValueExpr.VVar(varname) => varname.s
      case _ => arg.toString
    }
    "'" + s + "'"
  }
  override def toString : String = {
    val size = msg.size
    var s : String = ""
    var i : Int = 0
    do {
      val pos = msg.indexOf("%", i)
      if (pos < 0) {
        return s + msg.substring(i)
      } else {
        if (pos + 1 < size  && StringUtils.isASCIIDigit(msg(pos + 1))) {
          var j = pos + 2
          while (j < size && StringUtils.isASCIIDigit(msg(j))) j += 1
          val argIndex = msg.substring(pos + 1, j).toInt
          s = s + msg.substring(i, pos) + asString(args(argIndex))
          i = j
        } else {
          s = s + msg.substring(i, pos + 1)
          i = pos + 1
        }
      }
    } while (true)
    throw new RuntimeException("ErrorMessage.toString: internal error")
  }
}
