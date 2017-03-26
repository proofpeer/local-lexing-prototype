package net.proofpeer.locallexing.api

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
}
