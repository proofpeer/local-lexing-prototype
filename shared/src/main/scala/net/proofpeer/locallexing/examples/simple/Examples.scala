package net.proofpeer.locallexing.examples.simple

final object Examples {

  import net.proofpeer.locallexing.kernel._
  import Grammar._

  type P = Unit
  type EnvFunP = EnvFun[P]

  final val ENVFUNP : EnvFunP = (x : Environment[P]) => Some(()) 

  def rule(rhs : Symbol*) : Rule[P] = 
    Rule(ENVFUNP, rhs.map(sym => (sym, ENVFUNP)).toVector)

  def nonterminal(name : String, rules : Rule[P]*) : Nonterminal[P] = 
    Nonterminal(name, rules.toVector)

  def terminal(name : String, lexer : Lexer[Char, P]) : Terminal[Char, P] =
    Terminal(name, lexer)

  def makeParser[CHAR](grammar : Grammar[CHAR, P]) : Earley[CHAR, P] = {
    Grammar.check(grammar)
    val kernel = new Earley.Kernel(grammar)
    new Earley(kernel)
  }

  def mkGrammar(nonterminals : Vector[Nonterminal[P]], terminals : Vector[Terminal[Char, P]], selector : Selector[Char, P]) : Grammar[Char, P] = {
    Grammar[Char, P](nonterminals, terminals, (), selector)
  }

  def run(name : String, grammar : Grammar[Char, P], inputStr : String) {
    val parser = makeParser(grammar)
    val input = new StringInput(inputStr)
    val result = parser.parse(input)
    result match {
      case Left(parsetrees) => 
        println("Example: "+name)
        println("parsing of '" + inputStr + "' was successful")
        println("number of different parse trees (excluding cycles): " + ParseTree.countTrees(parsetrees : _*))
        val paths = ParseTree.collectPaths(parsetrees : _*).toVector
        println("number of different paths (excluding cycles): " + paths.size)
        for (p <- 1 to paths.size) {
          println("  " + p + ") " + ParseTree.printPath(grammar, input, paths(p-1)))  
        }
        println("")
      case Right(errorAt) => 
        println("Example: "+name)
        println("parsing of '" + inputStr + "' failed")
        println("parsing error found at position: " + errorAt)
        println("")
    }
  }

}