package cosc455.project1

// sorta like an interface but can provide implementation in it
trait LexicalAnalyzer {
  def getChar() : Char
  def getNextToken() : Unit
  def addChar() : Unit
  def lookup(candidate: String) : Boolean
}
