package cosc455.project1

object Compiler {

  var currentToken: String = ""

  val Scanner = new MyLexicalAnalyzer
  //val Parser = new MySyntaxAnalyzer

  def main(args:Array[String]) = {

    // get input filename
    val filename = args(0)

    val file_contents = scala.io.Source.fromFile(filename).mkString
    println(file_contents)

    // get the first token
    Scanner.start(file_contents)
    println(currentToken)

    Scanner.getNextToken()
    println(currentToken)


    /* old
    for (line <- scala.io.Source.fromFile(filename).getLines()) {
      // parse sentence against gittex grammar
      Parser.Gittex()

      // check for syntax errors
      if (Parser.getError)
        println("The sentence '" + line + "' does not follow the BNF grammar.")
      else
        println("The sentence '" + line + "' follows the BNF grammar.")
    }
    */
  }
}
