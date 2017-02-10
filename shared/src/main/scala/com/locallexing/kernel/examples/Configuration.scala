package com.locallexing.kernel.examples

import com.locallexing.kernel._

case class Configuration[CHAR](grammar : Grammar[CHAR], parser : Earley[CHAR], errorTerminals : Set[Grammar.TS])

object Configuration {

  def make[CHAR](grammar : Grammar[CHAR], errorTerminals : Set[Grammar.TS] = Set()) : Configuration[CHAR] = {
    val parser = GrammarUtils.makeParser(grammar)
    new Configuration(grammar, parser, errorTerminals)
  }


}
