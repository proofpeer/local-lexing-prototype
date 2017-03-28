package net.proofpeer.locallexing

package object api {

  trait Annotation

  abstract class Annotated {
    private var annotation_store : Option[Annotation] = null
    def annotation : Option[Annotation] = {
      if (annotation_store == null) None else annotation_store
    }
    def sealAnnotation(a : Option[Annotation]) {
      if (a == null || annotation_store != null) throw new RuntimeException("error sealing Annotation")
      annotation_store = a
    }
  }

  case class NameSegment(s : String) extends Annotated

  case class ModuleName(segments : Vector[NameSegment]) extends Annotated

  case class Name(moduleName : ModuleName, localName : NameSegment) extends Annotated

  type FunType = (TypeExpr, TypeExpr)

  type VarName = NameSegment

  type FieldName = NameSegment

  case class TupleIndex(index : Int) extends Annotated

  final case class Env(param : TypeExpr, vars : Map[VarName, TypeExpr], layoutVars : Set[VarName], functions: Map[Name, FunType]) {
    def hasLayout(varname : VarName) : Boolean = layoutVars.contains(varname)
  }

  def updateEnv(env : Env, varname : VarName, ty : TypeExpr, hasGeometry : Boolean) : Env = {
    if (hasGeometry)
      Env(env.param, env.vars + (varname -> ty), env.layoutVars + varname, env.functions)
    else
      Env(env.param, env.vars + (varname -> ty), env.layoutVars - varname, env.functions)
  }

}
