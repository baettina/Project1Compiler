package cosc455.project1

import scala.collection.mutable.ListBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {
  var         sourceLine : String = ""
  var         position   : Int = 0
  private var nextChar   : Char = '\u0000'
  private var lexLength  : Int = 0
  private var lexems     : List[String] = List("\\BEGIN", "\\begin", "\\END", "\\end", "\\TITLE[", "\\title", "]",
                                               "#", "\\PARAB", "\\parab", "\\PARAE", "\\parae", "*", "+", "\\\\",
                                               "[", "(", ")", "![", "\\DEF[", "\\def[", "=", "\\USE[", "\\use[")
  private var lexeme     = new ListBuffer[Char]()

  /** start() is our initial call to our Lexical Analyzer
    *
    * It initializes the a list of legal tokens in our grammar
    * and gets the first character of our first token.
    */
  def start(line: String) = {
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
    * Builds the token char by char using the cases:
    *   if char is whitespace -> gets next non-whitespace char
    *   if char if a symbol   -> calls appropriate processing function then checks if its legal using lookup()
    *   if char is text       -> processes text until a symbol
    */
  override def getNextToken(): Unit = {
    lexLength = 0

    if(nextChar.isWhitespace) {getNonBlank()}

    if(CONSTANTS.symbols.contains(nextChar)) {
      if(nextChar == '\\') {procAnno()}
      else if(nextChar == '!') {procImg()}
      else {procSym()}

      // convert char list into string
      val newToken : String = lexeme.mkString
      if(lookup(newToken)) {
        Compiler.currentToken = newToken
      }
    }
    else if(!nextChar.isWhitespace) {
      procText()
      Compiler.currentToken = lexeme.mkString
    }
    else {
      println("Lexical error: Can't get next token")
    }
    lexeme.clear()
  }

  /** procSym() processes a symbol
    *
    * Creates a token of that symbol.
    * Symbols processed by this method are solo symbols that shouldn't be tokenized with other chars
    */
  def procSym(): Unit = {
    addChar()
    getChar()
  }

  /** procText() processes text
    *
    * Creates a token composed of valid chars until a keyword is reached.
    */
  def procText(): Unit = {
    while(!CONSTANTS.symbols.contains(nextChar) && nextChar != '\n')  {
      addChar()
      getChar()
    }
  }

  /** procAnno() processes annotations
    *
    * Annotations are tokens that begin with \ (document tags, title, paragraph tags, newline, variables)
    * The token is composed of valid chars until a whitespace is reached or open bracket.
    * Since title and variable tags end with an open bracket, the open bracket is also appended.
    */
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

  /** procImg() processes image symbol
    *
    * Needed specifically for the beginning of an image keyword.
    * This method creates a token that consists of two symbols.
    */
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
    * Checks to see if the candicate string is present in the list of legal tokens.
    */
  override def lookup(candidate: String): Boolean = {
    if(!lexems.contains(candidate)) {
      println("Lexical error: '" + candidate + "' is an illegal annotation.")
      System.exit(1)
      return false
    }
    return true
  }

  /** getNonBlank() retrieves first non-whitespace character
    *
    * Repeatedly gets a new char until a non-whitespace char is found.
    */
  def getNonBlank(): Unit = while(nextChar.isWhitespace) getChar()

}
