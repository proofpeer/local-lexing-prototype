package com.locallexing.kernel.examples

final object Examples {

  import com.locallexing.kernel._
  import Grammar._
  import GrammarUtils._

  def run(name : String, grammar : Grammar[Char], inputStr : String, showGraph : Boolean = true) {
    val parser = makeParser(grammar)
    val input = new StringInput(inputStr)
    val result = parser.parse(input)
    result match {
      case Left(parseTree) => 
        if (showGraph) {
          println("-------------------------------------")
          val graph = PrettyParseTrees.mkGraph(grammar, input, parseTree)
          println(PrettyParseTrees.asDot(name, graph))
          println("-------------------------------------")
          println("")
        }
        println("Example: "+name)
        println("parsing of '" + inputStr + "' was successful")
        println("number of different parse trees (excluding cycles): " + parseTree.countTrees)
        if (showGraph) {
          println("to visualise them, copy/paste above digraph into a file parsetree.dot and run:")
          println("")
          println("    dot2tex --preproc parsetree.dot | dot2tex > parsetree.tex")
          println("    pdflatex parsetree.tex")
          println("")
        }
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
