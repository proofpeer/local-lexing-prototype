package net.proofpeer.locallexing.api

final class Signature(val typeChecking : TypeChecking) {

  import TypeExpr._
  import typeChecking._
  import TypeChecking._

  type Method = (Vector[Vector[TypeExpr] => TypeExpr], Vector[TypeExpr] => TypeExpr)
  type Signature = Vector[Method]

  def constFunc(a : TypeExpr) : Vector[TypeExpr] => TypeExpr = (_ => a)

  def Method(result : TypeExpr)(args : TypeExpr*) : Method = {
    (args.map(constFunc _).toVector, _ => result)
  }

  def Method(f : (TypeExpr, TypeExpr) => TypeExpr)(args : TypeExpr*) : Method = {
    (args.map(constFunc _).toVector, types => f(types(0), types(1)))
  }

  def Method(result : Vector[TypeExpr] => TypeExpr)(args : (Vector[TypeExpr] => TypeExpr)*) : Method = {
    (args.toVector, result)
  }

  def apply(instances : Method*) : Signature = {
    instances.toVector
  }

  private def checkMethod(method : Method, env : Env, args : Vector[ValueExpr]) : 
    Either[TypeExpr, Vector[ErrorMessage]] = 
  {
    if (method._1.size != args.size) throw new RuntimeException("Signature.check: internal error")
    val types = args.map(v => typeValueExpr(env, v))
    var errors : Vector[ErrorMessage] = Vector()
    for (i <- 0 until types.size) {
      val ty = method._1(i)(types)
      if (!isSubtype(types(i), ty)) 
        errors = errors :+ ValueHasWrongType(args(i), types(i), ty)
    }
    if (errors.isEmpty)
      Left(method._2(types))
    else
      Right(errors)
  }

  def check(signature : Signature, env : Env, args : ValueExpr*) : TypeExpr = {
    var errors : Vector[ErrorMessage] = null
    val vargs = args.toVector
    for (method <- signature) {
      checkMethod(method, env, vargs) match {
        case Left(ty) => return ty
        case Right(errs) => 
          if (errors == null) errors = errs
          else if (errs.size < errors.size) errors = errs
      }
    }
    for (error <- errors) er.record(error)
    tnone
  }

}  





