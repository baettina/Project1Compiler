package cosc455.project1

object CONSTANTS {
  val DOCB       : String       = "\\BEGIN"
  val DOCE       : String       = "\\END"
  val TITLEB     : String       = "\\TITLE["
  val BRACKETE   : Char         = ']'
  val HEADING    : Char         = '#'
  val PARAB      : String       = "\\PARAB"
  val PARAE      : String       = "\\PARAE"
  val BOLD       : Char         = '*'
  val LISTITEM   : Char         = '+'
  val NEWLINE    : String        = "\\\\"
  val LINKB      : Char         = '['
  val ADDRESSB   : Char         = '('
  val ADDRESSE   : Char         = ')'
  val IMAGEB     : String       = "!["
  val DEFB       : String       = "\\DEF["
  val EQSIGN     : Char         = '='
  val USEB       : String       = "\\USE["

  val symbols    : List[Char]   = List('!', '#', '\\', '*', '+', '[', ']', '(' , ')', '=')
  val letters    : List[Char]   = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
                                       'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z')
  val numbersEtc : List[Char]   = List('1','2','3','4','5','6','7','8','9','0',',','.','\"',':','?','_','/', ''')
  val whiteSpace : List[Char]   = List(' ',  '\t', '\n', '\b','\f','\r')
  val validText  : List[Char]   = whiteSpace ::: letters ::: numbersEtc


}
