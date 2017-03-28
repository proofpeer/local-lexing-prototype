package net.proofpeer.locallexing.api

final class SignatureDefs(val Signature : Signature) {

  import TypeExpr._
  import Signature.typeChecking._
  import Signature.Method
  import Signature.constFunc
  import TypeChecking._

  private def projFirst(ty1 : TypeExpr, ty2 : TypeExpr) : TypeExpr = ty1

  final val sigIf = 
    Signature(Method(join _)(tany, tany, tboolean))

  final val sigCharacter =
    Signature(
      Method(tunit)(tinteger, tinteger), 
      Method(tunit)(tstring, tstring))

  final val sigEq = 
    Signature(Method(tboolean)(tany, tany))

  final val sigLeq = 
    Signature(
      Method(tboolean)(tinteger, tinteger), 
      Method(tboolean)(tstring, tstring),
      Method(tboolean)(tanyset, tanyset),
      Method(tboolean)(tanyset, tanymap))

  final val sigLess = sigLeq  

  final val sigAnd = Signature(Method(tboolean)(tboolean, tboolean))

  final val sigNot = Signature(Method(tboolean)(tboolean))

  final val sigPlus =
    Signature(
      Method(tinteger)(tinteger, tinteger),
      Method(tstring)(tstring, tstring),
      Method(join _)(tanyset, tanyset),
      Method(join _)(tanymap, tanymap),
      Method(join _)(tanyvector, tanyvector))

  final val sigMinus = 
    Signature(
      Method(tinteger)(tinteger, tinteger),
      Method(projFirst _)(tanyset, tanyset),
      Method(projFirst _)(tanymap, tanyset))

  final val sigMul =
    Signature(
      Method(tinteger)(tinteger, tinteger),
      Method(meet _)(tanyset, tanyset),
      Method(meet_map_set _)(tanymap, tanyset))

  final val sigDiv = Signature(Method(tinteger)(tinteger, tinteger))

  final val sigMod = Signature(Method(tinteger)(tinteger, tinteger))

  final val sigSize = 
    Signature(
      Method(tinteger)(tanyset),
      Method(tinteger)(tanymap),
      Method(tinteger)(tanyvector))

  final val sigApply = 
    Signature(
      Method(v => targetTypeOf(v(0)))(constFunc(tanymap), v => domainTypeOf(v(0))),
      Method(constFunc(tboolean))(constFunc(tanyset), v => elemTypeOf(v(0))),
      Method(v => elemTypeOf(v(0)))(constFunc(tanyvector), constFunc(tinteger)))

}