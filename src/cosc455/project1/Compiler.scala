package cosc455.project1

import java.awt.Desktop
import java.io.{File, IOException}


/** Handles the compilation process
  *
  *  @constructor initializes the three vital parts of the compiler:
  *               MyLexicalAnalyzer, MySyntaxAnalyzer, MySemanticAnalyzer
  *
  */
object Compiler {
  var currentToken: String = ""
  var fileContents: String = ""
  var gittexTokens = List[String]()
  var filename: String = ""

  val Scanner    = new MyLexicalAnalyzer
  val Parser     = new MySyntaxAnalyzer
  val Translator = new MySemanticAnalyzer

  /** main method
    *
    * First checks to see if the given parameter is has the valid file extention
    * then stores the file contents to a string.
    * This string is passed to our Scanner -> populates list of valid tokens, gittexTokens
    * Calls the Parser -> ensures the file conforms with our grammar
    * Starts the Translator -> generates an html file
    * Finally, displays the generated file using the user's default browser.
    */
  def main(args:Array[String]) = {
    checkFile(args)
    filename = args(0)
    readFile(args(0))
    println("Compiling " + filename + "...")

    Scanner.start(fileContents)
    Parser.gittex()
    Translator.start(filename)

    openHTMLFileInBrowser(filename + ".html")
  }

  /** checkFile
    *
    * Checks if file has the correct extension
    */
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

  /** readFile
    *
    * Stores the file contents to a string.
    */
  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  /** openHTMLFileInBrowser
    *
    * Scala/Java function to take a String filename and open in default web browser.
    */
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
