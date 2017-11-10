package cosc455.project1

import scala.collection.mutable.{ListBuffer, Map}

class MySemanticAnalyzer {

  // create the hashmap here maybe
  var token: String = ""
  var position: Int = 0
  var html = ListBuffer[String]()
  var gVars = Map[String, String]()
  var pVars = Map[String, String]()

  def getNextToken(): Unit = {
    token = Compiler.gittexTokens(position)
    position += 1
  }

  def start(): Unit = {
    getNextToken()
    gittex()

    for (elem <- html) {print(elem)}
  }

  def gittex(): Unit = {
    if (token.equalsIgnoreCase(CONSTANTS.DOCB)) {
      html += "<html>\n"
      getNextToken()

      if (token.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }

      if (token.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        title()
        body()

        if (token.equalsIgnoreCase(CONSTANTS.DOCE)) {
          html += "\n</html>"

        }
      }
    }
  }

  def title(): Unit = {
    if(token.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      html += "<title>"
      getNextToken()

      if (isValidText(token)) {
        html += token
        getNextToken()

        if (token.equals(CONSTANTS.BRACKETE.toString)) {
          html += "</title>\n"
          getNextToken()
        }
      }
    }
  }

  def body(): Unit = {
    if(token.equals(CONSTANTS.USEB) || token.equals(CONSTANTS.HEADING.toString) ||
      token.equals(CONSTANTS.BOLD.toString) || token.equals(CONSTANTS.LISTITEM.toString) ||
      token.equals(CONSTANTS.IMAGEB.toString) || token.equals(CONSTANTS.LINKB.toString) ||
      isValidText(token)) {
      innertext()
      body()
    }

    if(token.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph()
      body()
    }

    if(token.equals(CONSTANTS.NEWLINE.toString)) {
      newline()
      body()
    }
  }

  def innertext(): Unit = {
    if(token.equals(CONSTANTS.USEB)) {
      variableUse()
      innertext()
    }
    if(token.equals(CONSTANTS.HEADING.toString)) {
      heading()
      innertext()
    }
    if(token.equals(CONSTANTS.BOLD.toString)) {
      bold()
      innertext()
    }
    if(token.equals(CONSTANTS.LISTITEM.toString)) {
      listItem()
      innertext()
    }
    if(token.equals(CONSTANTS.IMAGEB)) {
      image()
      innertext()
    }
    if(token.equals(CONSTANTS.LINKB.toString)) {
      link()
      innertext()
    }
    if(isValidText(token)) {
      html += token
      getNextToken()
      innertext()
    }
  }

  def paragraph(): Unit = {
    if(token.equalsIgnoreCase(CONSTANTS.PARAB)) {
      html += "<p>\n"
      getNextToken()

      if(token.equals(CONSTANTS.DEFB)) {
        variableDefine()
      }

      if(!token.equals(CONSTANTS.PARAE)) {
        innertext()
      }

      if(token.equals(CONSTANTS.PARAE)) {
        html += "\n</p>\n"
        getNextToken()
      }
    }
  }

  def heading(): Unit = {
    if(token.equals(CONSTANTS.HEADING.toString)) {
      html += "<h1>"
      getNextToken()

      if(isValidText(token)){
        html += token.toList.filter(_ != '\n').mkString
        html += "</h1>\n"
        getNextToken()
      }
    }
  }

  def variableDefine(): Unit = {
    var varName: String = ""

    if (token.equalsIgnoreCase(CONSTANTS.DEFB)) {
      getNextToken()

      // check for REQTEXT
      if(isValidText(token)) {
        // the token should be variable name
        varName = token.toList.filter(!_.isWhitespace).mkString
        getNextToken()

        if(token.equals(CONSTANTS.EQSIGN.toString)) {
          getNextToken()

          if(isValidText(token)) {
            gVars + (varName -> token)
            getNextToken()

            if(token.equals(CONSTANTS.BRACKETE.toString)) {
              getNextToken()

              variableDefine()
            }
          }
        }
      }
    }
  }

  def variableUse(): Unit = {
    if(token.equalsIgnoreCase(CONSTANTS.USEB)) {
      getNextToken()

      // next token should be the variable name - check this in the semantic analyzer
      if(isValidText(token)) {
        if(gVars.contains(token.toList.filter(!_.isWhitespace).mkString)){
          html += gVars(token)
        }
        getNextToken()

        if(token.equals(CONSTANTS.BRACKETE.toString)) {
          getNextToken()
        }
      }
    }
  }

  def bold(): Unit = {
    if(token.equals(CONSTANTS.BOLD.toString)) {
      html += "<b>"
      getNextToken()

      if(isValidText(token)) {
        html += token
        getNextToken()

        if(token.equals(CONSTANTS.BOLD.toString)) {
          html += "</b>"
          Compiler.Scanner.getNextToken()
        }
      }
    }
  }

  def listItem(): Unit = {
    if (token.equals(CONSTANTS.LISTITEM.toString)) {
      html += "<li>"
      getNextToken()

      inneritem()
      html += "</li>\n"

      if(token.equals(CONSTANTS.LISTITEM))
        listItem()
    }
  }

  def inneritem(): Unit = {
    if(token.equals(CONSTANTS.USEB)) {
      variableUse()
      inneritem()
    }
    if(token.equals(CONSTANTS.BOLD.toString)) {
      bold()
      inneritem()
    }
    if(token.equals(CONSTANTS.LINKB.toString)) {
      link()
      inneritem()
    }
    if(isValidText(token)) {
      html += token.toList.filter(_ != '\n').mkString
      getNextToken()
      inneritem()
    }

  }

  def link(): Unit = {
    var lname: String = ""

    if (token.equals(CONSTANTS.LINKB.toString)) {
      getNextToken()

      if(isValidText(token)) {
        lname = token
        getNextToken()

        if(token.equals(CONSTANTS.BRACKETE.toString)) {
          getNextToken()

          if(token.equals(CONSTANTS.ADDRESSB.toString)) {
            getNextToken()

            if(isValidText(token)) {
              html += "<a href=\"" + token + "\"> " + lname + " </a>"
              getNextToken()

              if(token.equals(CONSTANTS.ADDRESSE.toString)) {
                getNextToken()
              }
            }
          }
        }
      }
    }
  }

  def image(): Unit = {
    var lname: String = ""
    if(token.equals(CONSTANTS.IMAGEB)) {
      getNextToken()

      if(isValidText(token)) {
        lname = token
        getNextToken()

        if(token.equals(CONSTANTS.BRACKETE.toString)) {
          getNextToken()

          if(token.equals(CONSTANTS.ADDRESSB.toString)) {
            getNextToken()

            if(isValidText(token)) {
              html += "<img src =\"" + token + "\" alt=\"" + lname + "\">"
              getNextToken()

              if(token.equals(CONSTANTS.ADDRESSE.toString)) {
                getNextToken()
              }
            }
          }
        }
      }
    }
  }

  def newline(): Unit = {
    if(token.equals(CONSTANTS.NEWLINE.toString)){
      html += "<br>\n"
      getNextToken()
    }
  }

  def isValidText(candidate: String): Boolean = {
    candidate.toList.forall(x => CONSTANTS.validText.contains(x))
  }
}
