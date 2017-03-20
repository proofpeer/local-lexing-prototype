package net.proofpeer.locallexing.kernel.examples.simple

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

  def terminal[CHAR](name : String, lexer : Lexer[CHAR, P]) : Terminal[CHAR, P] =
    Terminal(name, lexer)

  def makeParser[CHAR](grammar : Grammar[CHAR, P]) : Earley[CHAR, P] = {
    Grammar.check(grammar)
    val kernel = new Earley.Kernel(grammar)
    new Earley(kernel)
  }

  def run(name : String, grammar : Grammar[Char, P], inputStr : String) {
    val parser = makeParser(grammar)
    val input = new StringInput(inputStr)
    val result = parser.parse(input)
    result match {
      case Left(parseTree) => 
        println("Example: "+name)
        println("parsing of '" + inputStr + "' was successful")
        println("number of different parse trees (excluding cycles): " + parseTree.countTrees)
        val paths = ParseTree.collectPaths(parseTree).toVector
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