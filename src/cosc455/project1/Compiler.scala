package cosc455.project1

object Compiler {

  var currentToken: String = ""
  var fileContents: String = ""

  val Scanner = new MyLexicalAnalyzer
  //val Parser = new MySyntaxAnalyzer

  def main(args:Array[String]) = {

    // get input
    checkFile(args)
    readFile(args(0))

    // get the first token
    Scanner.start(fileContents)
    println(currentToken)

    while(currentToken != Nil){
      Scanner.getNextToken()
      println(currentToken)
    }

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
  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args. Expected number of args is 1")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension. File must be .gtx")
      System.exit(1)
    }
  }
}
