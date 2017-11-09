package cosc455.project1

object CONSTANTS {
  val DOCB       : String = "\\BEGIN"
  val DOCE       : String = "\\END"
  val TITLEB     : String = "\\TITLE["
  val BRACKETE   : Char   = ']'
  val HEADING    : Char   = '#'
  val PARAB      : String = "\\PARAB"
  val PARAE      : String = "\\PARAE"
  val BOLD       : Char   = '*'
  val LISTITEM   : Char   = '+'
  val NEWLINE    : Char   = '\\'
  val LINKB      : Char   = '['
  val ADDRESSB   : Char   = '('
  val ADDRESSE   : Char   = ')'
  val IMAGEB     : String = "!["
  val DEFB       : String = "\\DEF["
  val EQSIGN     : Char   = '='
  val USEB       : String = "\\USE["

  val symbols    : List[Char] = List('!', '#', '\\', '*', '+', '[', ']', '(' , ')', '=')
  val letters    : List[String] = List("a","b","c","d","e","f","g","h","i","j","k","l","m",
    "n","o","p","q","r","s","t","u","v","w","x","y","z")
  val numbersEtc : List[String] = List("1","2","3","4","5","6","7","8","9","0",
    ",",".","\"",":","?","_","/", "'", "")
  val whiteSpace : List[String] = List(" ", "\t", "\n", "\b","\f","\r")
  val validText  : List[String] = whiteSpace ::: letters ::: numbersEtc


}
