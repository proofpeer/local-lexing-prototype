package net.proofpeer.locallexing

package object api {

  type ModuleName = Vector[String]

  type Name = (ModuleName, String)

  //type FunType = (TypeExpr, TypeExpr)

  type VarName = String

  type Env = (TypeExpr, Map[VarName, TypeExpr])

  def updateEnv(env : Env, varname : VarName, ty : TypeExpr) : Env = {
    (env._1, env._2 + (varname -> ty))
  }

}
