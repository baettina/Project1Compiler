package cosc455.project1

import scala.collection.mutable.ListBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {
  private var sourceLine : String = ""
  private var lexeme = new ListBuffer[Char]()
  private var nextChar : Char = '\u0000'
  private var lexLength : Int = 0
  private var position : Int = 0
  private var lexems : List[String] = List()
  private val plaintext = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "," ++ "." ++ "\"" ++ ":" ++ "?" ++ "_" ++ "/").toSet
  private val keyword = List('#', '+', '*', '=', '[', ']', '(', ')', '!')
  private val whitespace = List(' ', '\t', '\n', '\b','\f','\r')

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

    // get first non blank char
    getNonBlank()

    if (nextChar == '\\'){
      getAnnotation()
    }
    else if (keyword.contains(nextChar)){
      getSolo()
    }
    else if (plaintext.contains(nextChar)){
      //process text until next keyword
      // try this >> nextChar.isWhitespace
      while(!isSpace(nextChar) || !keyword.contains(nextChar)){
        addChar()
        getChar()
      }
    }

    /*this works but it doesnt catch link [ and ] and ( and )
while(!whitespace.contains(nextChar) && nextChar != '['){
addChar()
getChar()
}
if (nextChar == '['){
addChar()
getChar()
}*/



    // convert char list into string
    var newToken : String = lexeme.mkString

    // only uses look up for tokens that start with \
    if (lexeme.head == '\\' && lookup(newToken))
      Compiler.currentToken = newToken
    else
      Compiler.currentToken = newToken

    // clear the lexeme list ? ... yeah i think u gotta...
    lexeme.clear()
  }

  def getSolo(): Unit = {
    // nextChar is # + * = [ ] ( ) !
    addChar()
    getNonBlank()
  }

  def getAnnotation(): Unit = {
    // nextChar is \
    addChar()
    getChar()

    // continue until [
    while(nextChar != '[' || nextChar != ' '){
      addChar()
      getChar()
    }

    // add [ too
    if (nextChar == '['){
      addChar()
      getChar()
    }
  }

  /** addChar() adds current char to the token we are building
    *
    * our current char is appended to the List the length of the token is incremented by 1
    */
  override def addChar(): Unit = {
    lexeme += nextChar
    lexLength += 1
  }

  /** lookup() check to see if found token is a valid token
    *
    *
    */
  override def lookup(candidate: String): Boolean = {
    if(!lexems.contains(candidate)) {
      //Compiler.Parser().setError();
      if (!isPlaintext(candidate)) {
        println("LEXICAL ERROR - '" + candidate + "' contains characters that are not allowed.")
      }
      else {
        println("LEXICAL ERROR - '" + candidate + "' is not recognized.")
        return false
      }
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
