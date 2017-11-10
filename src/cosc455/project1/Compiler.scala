package cosc455.project1

import java.io._

object Compiler {

  var currentToken: String = ""
  var fileContents: String = ""
  var gittexTokens = List[String]()
  var filename: String = ""

  val Scanner    = new MyLexicalAnalyzer
  val Parser     = new MySyntaxAnalyzer
  val Translator = new MySemanticAnalyzer


  def main(args:Array[String]) = {

    // get input
    checkFile(args)
    filename = args(0)
    readFile(args(0))

    // get the first token
    println("Compiling " + filename + "...")
    Scanner.start(fileContents)
    Parser.gittex()

    // syntax is correct
    Translator.start(filename)
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
