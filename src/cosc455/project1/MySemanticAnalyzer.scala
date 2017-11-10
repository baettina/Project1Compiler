package cosc455.project1

import scala.collection.mutable.{ListBuffer, Map, Queue}

class MySemanticAnalyzer {

  // create the hashmap here maybe

  var html = ListBuffer[String]()
  var tokenlist  = List[String]()
  var variables  = Queue[String]()
  var vnames     = List[String]()
  var gVar = Map[String,String]()
  var pVar = Map[String,String]()
  var position = 0

  def start(gtokens: List[String]): Unit =  {
    tokenlist = gtokens
    //variables = Compiler.Parser.varnodes
    //variables.keys

    while(!tokenlist(position).equalsIgnoreCase(CONSTANTS.DOCE)){
      convert()
    }

    if(tokenlist(position).equalsIgnoreCase(CONSTANTS.DOCE)) {
      cEnd()
    }

    for (elem <- html) {print(elem)}
  }

  def convert(): Unit = {
    if(tokenlist(position).equalsIgnoreCase(CONSTANTS.DOCB))         {cBeg()}
    else if(tokenlist(position).equalsIgnoreCase(CONSTANTS.DEFB))    {cVdef()}
    else if(tokenlist(position).equalsIgnoreCase(CONSTANTS.USEB))    {cVuse()}
    else if(tokenlist(position).equalsIgnoreCase(CONSTANTS.TITLEB))  {cTitle()}
    else if(tokenlist(position).equals(CONSTANTS.HEADING.toString))  {cHeading()}
    else if(tokenlist(position).equalsIgnoreCase(CONSTANTS.PARAB))   {cPar()}
    else if(tokenlist(position).equals(CONSTANTS.LINKB.toString))    {cLink()}
    else if(tokenlist(position).equals(CONSTANTS.BOLD.toString))     {cBold()}
    else if(tokenlist(position).equals(CONSTANTS.LISTITEM.toString)) {cList()}
    else if(tokenlist(position).equals(CONSTANTS.IMAGEB))            {cImg()}
    else if(tokenlist(position).equals(CONSTANTS.NEWLINE))           {cNl()}
    else                                                             {text()}
  }

  def addNode(key: String, value: String) = {
    vnames = key.toList.filter(!_.isWhitespace).mkString :: vnames
    variables.enqueue(value)

  }

  def cVdef(): Unit = {
    position += 1
    var name = tokenlist(position)
    position += 2
    gVar + (name -> tokenlist(position))
    position += 2
  }

  def cVuse(): Unit = {
    position += 1
    val key: String = tokenlist(position)
    if(gVar.contains(key)) {
      // variable name found
      html += gVar(key)
    }
    position += 2
  }

  def cBeg(): Unit = {
    html += "<html>\n"
    position += 1
  }

  def cEnd(): Unit = {
    html += "\n</html>"
  }

  def cTitle(): Unit = {
    html += "<title>"
    position += 1
    html += tokenlist(position)
    position += 1
    html += "</title>\n"
    position += 1
  }

  def cHeading(): Unit = {
    html += "<h1>"
    position += 1
    html += tokenlist(position).toList.filter(_ != '\n').mkString
    position += 1
    html += "</h1>\n"
  }

  def cPar(): Unit = {
    html += "<p>"
    position += 1
    cPin()
    html += "</p>\n"
    position += 1
  }

  def cPin(): Unit = {
    if(tokenlist(position).equalsIgnoreCase(CONSTANTS.DEFB))         {cPvdef()}
    else if(tokenlist(position).equalsIgnoreCase(CONSTANTS.USEB))    {cPvuse()}
    else if(tokenlist(position).equals(CONSTANTS.LINKB.toString))    {cLink()}
    else if(tokenlist(position).equals(CONSTANTS.BOLD.toString))     {cBold()}
    else if(tokenlist(position).equals(CONSTANTS.LISTITEM.toString)) {cList()}
    else if(tokenlist(position).equals(CONSTANTS.IMAGEB))            {cImg()}
    else                                                             {text()}

    if(!tokenlist(position).equalsIgnoreCase(CONSTANTS.PARAE)) {
      cPin()
    }
  }

  def cPvdef(): Unit = {
    position += 1
    var name = tokenlist(position)
    position += 2
    pVar + (name -> tokenlist(position))
    position += 2
  }

  def cPvuse(): Unit = {
    position += 1
    val key: String = tokenlist(position)
    if(pVar.contains(key)) {
      // variable name found
      html += pVar(key)
    }
    else if(gVar.contains(key)) {
      html += gVar(key)
    }
    else {
      println("Error: variable not defined")
    }
    position += 2

  }

  def cPBeg(): Unit = {
    html += "<p>"
    position += 1
  }

  def cPEnd(): Unit = {
    html += "</p>\n"
    position += 1
  }

  def text(): Unit = {
    html += tokenlist(position)
    position += 1
  }

  def cLink(): Unit = {
    position += 1 //[
    var title = tokenlist(position)
    position += 3 //title ] (
    html += "<a href=\"" + tokenlist(position) + "\">" + title + "</a>"
    position += 2
  }

  def cBold(): Unit = {
    html += "<b>"
    position += 1
    html += tokenlist(position).toList.filter(_ != '\n').mkString
    position += 1
    html += "</b>"
    position += 1
  }

  def cList(): Unit = {
    html += "<li>"
    position += 1
    listText()
    position += 1
    html += "</li>\n"
  }

  def listText(): Unit = {
    if(tokenlist(position).equalsIgnoreCase(CONSTANTS.USEB))         {cVuse()}
    else if(tokenlist(position).equals(CONSTANTS.LINKB.toString))    {cLink()}
    else if(tokenlist(position).equals(CONSTANTS.BOLD.toString))     {cBold()}
    else {
      html += tokenlist(position).toList.filter(_ != '\n').mkString
      position += 1
    }

    if(!tokenlist(position).equals("\n")){
      listText()
    }
    else {
      position += 1
    }
  }

  def cImg(): Unit = {
    position += 1
    var title = tokenlist(position)
    position += 3
    html += "<img src=\"" + tokenlist(position) + "\" alt=\"" + title + "\">"
    position += 2
  }

  def cNl(): Unit = {
    html += "<br>\n"
    position += 1
  }

}
