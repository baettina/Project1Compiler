package cosc455.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  private var sourceLine : String = ""
  private var lexeme : List[Char] = List()
  private var nextChar : Char = '\u0000'
  private var lexLength : Int = 0
  private var position : Int = 0
  private var lexems : List[String] = List()
  private val plaintext = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "," ++ "." ++ "\"" ++ ":" ++ "?" ++ "_" ++ "/").toSet
  private val whitespace = List(' ', '\t', '\n', '\b','\f','\r')

  def start(line: String) = {
    initializeLexems()
    sourceLine = line
    position = 0
    getNextToken()
  }

  // gets the next character
  override def getChar(): Char = {
    if (position < sourceLine.length()) {
      nextChar = sourceLine.charAt(position)
      position += 1
      return nextChar
    }
    else nextChar = '\n'.charValue()
    return nextChar
  }

  // looks at each char to get next token and sets it to the Compiler's currentToken
  override def getNextToken(): Unit = {
    lexLength = 0

    // add first char to token
    getNonBlank()
    addChar()
    getChar()

    // continue gathering characters for the token until empty space
    while(!whitespace.contains(nextChar)) {
      addChar()
      getChar()
    }

    // convert char list into string
    var newToken : String = (lexeme.reverse).mkString
    // set CurrentToken = newToken
    Compiler.currentToken = newToken
    // clear the lexeme list ?

    /*/-- do we need this?
    if(lookup(newToken))
      Compiler.currentToken = newToken*/
  }

  // adds current character to the token
  override def addChar(): Unit = {
    if(lexLength <= 98) {
      lexeme ::= nextChar
      lexLength += 1
    }
    else {
      println("LEXICAL ERROR - The found lexeme is too long!")

      if(!isSpace(nextChar))
        while(!isSpace(nextChar))
          getChar()

      lexLength = 0
      getNonBlank()
      addChar()
    }
  }

  override def lookup(candidate: String): Boolean = {
    if(!lexems.contains(candidate)) {
      //Compiler.Parser().setError();
      println("LEXICAL ERROR - '" + candidate + "' is not recognized.")
      return false
    }
    else if (!isPlaintext(candidate)) {
      println("LEXICAL ERROR - '" + candidate + "' contains characters that are not allowed.")
    }
    return true
  }

  def initializeLexems() = {
    lexems = List("\\BEGIN", "\\END", "\\TITLE[", "]", "#", "\\PARAB", "\\PARAE", "*", "+", "\\\\",
      "[", "(", ")", "![", "\\DEF[", "=", "\\USE[")
  }

  def isSpace(c : Char): Boolean = whitespace.contains(c)

  def getNonBlank(): Unit = while(isSpace(nextChar)) getChar()

  def isPlaintext(candidate: String) = candidate.forall(plaintext.contains(_))

}
