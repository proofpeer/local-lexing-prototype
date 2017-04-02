package net.proofpeer.locallexing.api

class GrammarDSL {

  import scala.language.implicitConversions 
  import net.proofpeer.locallexing.utils.StringUtils

  // Implicit Conversions

  implicit def str2Namespace(s : String) : Namespace = {
    Namespace(s)
  } 

  implicit def str2Alias(s : String) : (LocalName, Option[LocalName]) = {
    (NameSegment(s), None)
  } 

  implicit def strpair2Alias(alias : (String, String)) : (LocalName, Option[LocalName]) = {
    (NameSegment(alias._1), Some(NameSegment(alias._2)))
  } 

  implicit def str2NameSegment(s : String) : NameSegment = NameSegment(s)

  implicit def str2Name(s : String) : Name = {
    val namespace = Namespace(s)
    Name(namespace.parent.get, namespace.lastComponent)
  }

  implicit def str2TypeExpr(s : String) : TypeExpr = TypeExpr.TCustom(s)

  implicit def lexerSeqArgNoBinder(lexer : LexerExpr) : (LexerExpr, Option[VarName]) = (lexer, None)

  implicit def lexerSeqArgBinder(binding : (LexerExpr, String)) : (LexerExpr, Option[VarName]) = (binding._1, Some(NameSegment(binding._2)))

  implicit def str2Lexer(s : String) : LexerExpr = LexerExpr.string(s)

  implicit def str2ValueExpr(s : String) : ValueExpr = ValueExpr.VVar(NameSegment(s))

  implicit def int2ValueExpr(i : Int) : ValueExpr = ValueExpr.VInteger(i)

  implicit def bool2ValueExpr(b : Boolean) : ValueExpr = ValueExpr.VBoolean(b)

  // GrammarElems

  def importSymbols(aliases : (LocalName, Option[LocalName])*)(source : Namespace) {
    addElem(Grammar.ImportSymbols(aliases.toVector, source))
  }

  def importTypes(aliases : (LocalName, Option[LocalName])*)(source : Namespace) {
    addElem(Grammar.ImportTypes(aliases.toVector, source))
  }

  def typedef(name : LocalName, ty : TypeExpr) {
    addElem(Grammar.TypeDef(name, ty))
  }

  def declT(name : LocalName, input_ty : TypeExpr = TypeExpr.tunit, output_ty : TypeExpr = TypeExpr.tunit, isPublic : Boolean = true, 
    isErrorTerminal : Boolean = false) 
  {
    addElem(Grammar.LexerDecl(name, isPublic, (input_ty, output_ty), isErrorTerminal))
  }

  def ruleT(name : LocalName, lexer : LexerExpr) {
    addElem(Grammar.LexerRule(name, lexer))
  }

  def defT(name : LocalName, input_ty : TypeExpr = TypeExpr.tunit, output_ty : TypeExpr = TypeExpr.tunit, isPublic : Boolean = true, 
    isErrorTerminal : Boolean = false)(lexer : LexerExpr) 
  {
    declT(name, input_ty, output_ty, isPublic, isErrorTerminal)
    ruleT(name, lexer)
  }

  def declN(name : LocalName, input_ty : TypeExpr = TypeExpr.tunit, output_ty : TypeExpr = TypeExpr.tunit, isPublic : Boolean = true) {
    addElem(Grammar.ParserDecl(name, isPublic, (input_ty, output_ty)))
  }

  def ruleN(name : LocalName, parser : ParserExpr) {
    addElem(Grammar.ParserRule(name, parser))
  }

  def defN(name : LocalName, input_ty : TypeExpr = TypeExpr.tunit, output_ty : TypeExpr = TypeExpr.tunit, isPublic : Boolean = true)(
    parser : ParserExpr)
  {
    declN(name, input_ty, output_ty, isPublic)
    ruleN(name, parser)
  }

  def lessThan(terminal1 : Name, terminal2 : Name) {
    addElem(Grammar.PriorityDecl(terminal1, terminal2))
  }

  // TypeExprs

  def tAny : TypeExpr = TypeExpr.TAny()

  def tNone : TypeExpr = TypeExpr.TNone()

  def tInt : TypeExpr = TypeExpr.TInteger()

  def tBool : TypeExpr = TypeExpr.TBoolean()

  def tStr : TypeExpr = TypeExpr.TString()

  def tRecord(fields : (FieldName, TypeExpr)*) : TypeExpr = TypeExpr.TRecord(fields.toMap)

  def tVec(elem : TypeExpr) : TypeExpr = TypeExpr.TVector(elem)

  def tSet(elem : TypeExpr) : TypeExpr = TypeExpr.TSet(elem)

  def tMap(domain : TypeExpr, target : TypeExpr) : TypeExpr = TypeExpr.TMap(domain, target)

  // LexerExprs

  def lFail : LexerExpr = LexerExpr.Fail()

  def lEOF : LexerExpr = LexerExpr.EOF()

  def lChar(min : ValueExpr, max : ValueExpr) : LexerExpr = LexerExpr.Character(min, max)

  def lChar(c : ValueExpr) : LexerExpr = LexerExpr.Character(c, c)

  def lOr(lexers : LexerExpr*) : LexerExpr = LexerExpr.choice(lexers : _*)

  def lSeq(lexers : (LexerExpr, Option[VarName])*) : LexerExpr = LexerExpr.Sequence(lexers.toVector, None)

  def lSeqR(lexers : (LexerExpr, Option[VarName])*)(returnValue : ValueExpr) : LexerExpr = 
    LexerExpr.Sequence(lexers.toVector, Some(returnValue))

  def lOpt(lexer : LexerExpr) : LexerExpr = LexerExpr.Optional(lexer)

  def lRep(lexer : LexerExpr) : LexerExpr = LexerExpr.Repeat(lexer)

  def lRep1(lexer : LexerExpr) : LexerExpr = LexerExpr.Repeat1(lexer)

  def lAnd(lexer : LexerExpr) : LexerExpr = LexerExpr.And(lexer)

  def lNot(lexer : LexerExpr) : LexerExpr = LexerExpr.Not(lexer)

  def lRevAnd(lexer : LexerExpr) : LexerExpr = LexerExpr.ReverseAnd(lexer)

  def lRevNot(lexer : LexerExpr) : LexerExpr = LexerExpr.ReverseNot(lexer)

  def lWord(lexer : LexerExpr) : LexerExpr = LexerExpr.Word(lexer)

  def lLine(lexer : LexerExpr) : LexerExpr = LexerExpr.Line(lexer)

  def lParagraph(lexer : LexerExpr) : LexerExpr = LexerExpr.Paragraph(lexer)

  def lRowGeq(lexer : LexerExpr, k : ValueExpr) : LexerExpr = LexerExpr.RowGeq(lexer, k)

  def lRowLeq(lexer : LexerExpr, k : ValueExpr) : LexerExpr = LexerExpr.RowLeq(lexer, k)

  def lColGeq(lexer : LexerExpr, k : ValueExpr) : LexerExpr = LexerExpr.ColumnGeq(lexer, k)

  def lColLeq(lexer : LexerExpr, k : ValueExpr) : LexerExpr = LexerExpr.ColumnLeq(lexer, k)

  def L(name : Name) : LexerExpr = LexerExpr.Call(name, ValueExpr.VUnit())

  def L(name : Name, param : ValueExpr) = LexerExpr.Call(name, param)

  // ParserExprs

  def pFail : ParserExpr = ParserExpr.Lexer(lFail)

  def pOr(parsers : ParserExpr*) : ParserExpr = ParserExpr.choice(parsers : _*)

  def pSeq(parsers : (ParserExpr, Option[VarName])*) : ParserExpr = ParserExpr.Sequence(parsers.toVector, None)

  def pSeqR(parsers : (ParserExpr, Option[VarName])*)(returnValue : ValueExpr) : ParserExpr = 
    ParserExpr.Sequence(parsers.toVector, Some(returnValue))

  def pOpt(parser : ParserExpr) : ParserExpr = ParserExpr.Optional(parser)

  def pRep(parser : ParserExpr) : ParserExpr = ParserExpr.Repeat(parser)

  def pRep1(parser : ParserExpr) : ParserExpr = ParserExpr.Repeat1(parser)

  def P(name : Name) : ParserExpr = ParserExpr.Call(name, ValueExpr.VUnit())

  def P(name : Name, param : ValueExpr) : ParserExpr = ParserExpr.Call(name, param)

  def pL(lexer : LexerExpr) : ParserExpr = ParserExpr.Lexer(lexer)

  // ValueExprs (also see methods in ValueExpr class)

  def UNIT : ValueExpr = ValueExpr.VUnit()

  def THIS : ValueExpr = ValueExpr.VThis()

  def V(v : ValueExpr) : ValueExpr = v

  def fail : ValueExpr = ValueExpr.VFail()

  def abort : ValueExpr = ValueExpr.VAbort()

  def str(s : String) : ValueExpr = ValueExpr.VString(StringUtils.codePoints(s))

  def int(i : Int) : ValueExpr = i

  def tuple(elems : ValueExpr*) : ValueExpr = ValueExpr.VTuple(elems.toVector)

  def record(fields : (String, ValueExpr)*) : ValueExpr = 
    ValueExpr.VRecord(fields.map(f => (NameSegment(f._1) -> f._2)).toMap)

  def vector(elems : ValueExpr*) : ValueExpr = ValueExpr.VVector(elems.toVector)

  def set(elems : ValueExpr*) : ValueExpr = ValueExpr.VSet(elems.toVector)

  def map(mappings : (ValueExpr, ValueExpr)*) : ValueExpr = ValueExpr.VMap(mappings.toVector)

  def vIf(cond : ValueExpr, vtrue : ValueExpr, vfalse : ValueExpr) : ValueExpr = ValueExpr.VIf(cond, vtrue, vfalse)

  def size(col : ValueExpr) : ValueExpr = ValueExpr.VSize(col)

  def dispatch(binding : (ValueExpr, String))(cases : (TypeExpr, ValueExpr)*) : ValueExpr = 
    ValueExpr.VDispatch(binding._1, binding._2, cases.toVector)

  def let(bindings : (ValueExpr, String)*)(value : ValueExpr) : ValueExpr = 
    ValueExpr.VLet(bindings.map(b => (b._1, NameSegment(b._2))).toVector, value)

  private def vLayout(v : String, layout : LayoutExpr) : ValueExpr = ValueExpr.VLayout(NameSegment(v), layout)

  def firstCol(v : String) = vLayout(v, LayoutExpr.FirstCol())

  def firstRow(v : String) = vLayout(v, LayoutExpr.FirstRow())

  def lastCol(v : String) = vLayout(v, LayoutExpr.LastCol())

  def lastRow(v : String) = vLayout(v, LayoutExpr.LastRow())

  def leftMost(v : String) = vLayout(v, LayoutExpr.LeftMost())

  // Other stuff

  private var elems : Vector[GrammarElem] = Vector()
  private def addElem(elem : GrammarElem) {
    elems = elems :+ elem
  }

  def toGrammar : Grammar = Grammar(elems)

}

final class GrammarRegistry {

  def add(namespace : Namespace, grammar : Grammar) {
    if (!namespace.isAbsolute) throw new RuntimeException("Grammar namespaces must be absolute.")
    if (grammars_.get(namespace).isDefined) throw new RuntimeException("Grammar namespace is already registered.")
    grammars_ = grammars_ + (namespace -> grammar)
  }

  def add(namespace : Namespace, dsl : GrammarDSL) {
    add(namespace, dsl.toGrammar)
  }

  def grammars : Map[Namespace, Grammar] = grammars_

  private var grammars_ : Map[Namespace, Grammar] = Map()

}

object TestDSL {

  def main(args : Array[String]) {
    val g = new GrammarDSL()
    import g._
    importSymbols("sin", "cos" -> "c", "tan")("\\obua\\math")
    defT("hello")(LexerExpr.Fail()) 
  }

}