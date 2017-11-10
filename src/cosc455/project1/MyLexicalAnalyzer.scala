package cosc455.project1

import scala.collection.mutable.ListBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {
  var sourceLine : String = ""
  var position : Int = 0
  private var lexeme = new ListBuffer[Char]()
  private var nextChar : Char = '\u0000'
  private var lexLength : Int = 0
  private var lexems : List[String] = List()
  private val plaintext = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "," ++ "." ++ "\"" ++ ":" ++ "?" ++ "_" ++ "/").toSet

  /** start() is our initial call to our Lexical Analyzer
    *
    * It initializes the certain variables used throughout the program
    * and gets the first character of our first token.
    */
  def start(line: String) = {
    initializeLexems()
    sourceLine = line
    position = 0
    getChar()
    getNextToken()
  }

  /** getChar() retrieves the next character
    *
    * Checks to see if current position is still within the bounds of our input file
    * then takes the charAt(position) and increments position
    */
  // gets the next character
  override def getChar(): Char = {
    if (position < sourceLine.length()) {
      nextChar = sourceLine.charAt(position)
      position += 1
      return nextChar
    }
    return nextChar
  }

  /** getNextToken() builds a token and sets it to Compiler.currentToken
    *
    * Initializes token length to 0 and looks at each char and appends it to a List, lexeme
    * until a whitespace. This List is converted to a string which we verify using lookup().
    * The contents of this List is cleared after.
    */
  override def getNextToken(): Unit = {
    lexLength = 0

    if(nextChar.isWhitespace) {getNonBlank()}

    if(CONSTANTS.symbols.contains(nextChar)) {
      if(nextChar == '\\') {procAnno()}
      else if(nextChar == '!') {procImg()}
      else {procSym()}

      // convert char list into string
      var newToken : String = lexeme.mkString

      if(lookup(newToken)) {
        Compiler.currentToken = newToken
      }
    }
    else if(!nextChar.isWhitespace) {
      procText()
      Compiler.currentToken = lexeme.mkString
    }
    else {
      println("Error: Can't get next token")
    }
    lexeme.clear()
  }


  def procSym(): Unit = {
    addChar()
    getChar()
  }

  def procText(): Unit = {
    while(!CONSTANTS.symbols.contains(nextChar))  {
      addChar()
      getChar()
    }
  }

  def procAnno(): Unit = {
    if(nextChar == '\\'){
      addChar()
      getChar()

      while(!nextChar.isWhitespace && nextChar != '[') {
        addChar()
        getChar()
      }

      if(nextChar == '[') {
        addChar()
        getChar()
      }
    }
  }

  def procImg(): Unit = {
    if(nextChar == '!') {
      addChar()
      getChar()

      if(nextChar == '[') {
        addChar()
        getChar()
      }
    }
  }

  /** addChar() adds current char to the token we are building
    *
    * The current char is appended to the List,
    * the length of the token is incremented by 1
    */
  override def addChar(): Unit = {
    lexeme += nextChar
    lexLength += 1
  }

  /** lookup() check to see if found token is a valid token
    *
    * First, it checks if the annotation or symbol is recognized.
    * If not, it checks if contains illegal characters
    * else, it declares that the candidate is an illegal annotation
    */
  override def lookup(candidate: String): Boolean = {
    if(!lexems.contains(candidate)) {
      println("LEXICAL ERROR - '" + candidate + "' is an illegal annotation.")
      System.exit(1)
      return false
    }
    return true
  }

  def initializeLexems() = {
    lexems = List("\\BEGIN", "\\begin", "\\END", "\\end", "\\TITLE[", "\\title", "]", "#", "\\PARAB", "\\parab", "\\PARAE", "\\parae", "*", "+", "\\\\",
                  "[", "(", ")", "![", "\\DEF[", "\\def[", "=", "\\USE[", "\\use[")
  }

  def getNonBlank(): Unit = while(nextChar.isWhitespace) getChar()

  def isPlaintext(candidate: String) = candidate.forall(plaintext.contains(_))

}
