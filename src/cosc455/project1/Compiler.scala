package cosc455.project1

import java.awt.Desktop
import java.io.{File, IOException}

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
    openHTMLFileInBrowser(filename + ".html")
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

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}
