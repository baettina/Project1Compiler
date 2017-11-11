package cosc455.project1

import java.io.{File, PrintWriter}
import scala.collection.mutable.Map

/**
  * Coverts gittex file to html
  */
class MySemanticAnalyzer {

  var token    : String = ""
  var position : Int    = 0
  //var html              = ListBuffer[String]()
  var gVars             = Map[String, String]()
  var pVars             = Map[String, String]()
  val htmlfile          = new PrintWriter(new File("index.html"))


  /** start() begins our translation
    *
    * This method sets a value for token and calls the translation start state.
    * After translating, it closes the file we are writing to and renames the file
    * according to the requirements.
    */
  def start(filename: String): Unit = {
    getNextToken()
    gittex()

    htmlfile.close()
    new File("index.html").renameTo(new File(filename + ".html"))
  }

  /** getNextToken() retrieves a new token
    *
    * Gets a token from the list of valid tokens and increments positions by 1
    */
  def getNextToken(): Unit = {
    token = Compiler.gittexTokens(position)
    position += 1
  }

  /** gittex() is the translation's start state
    *
    * Translates the document tags to <html> </html>
    */
  def gittex(): Unit = {
    if (token.equalsIgnoreCase(CONSTANTS.DOCB)) {
      htmlfile.write("<html>\n")
      getNextToken()

      if (token.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }

      if (token.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        title()
        body()

        if (token.equalsIgnoreCase(CONSTANTS.DOCE)) {
          htmlfile.write("\n</html>")
        }
      }
    }
  }

  /** title()
    *
    * Translates the title to the format:
    * <title> title text </title>
    */
  def title(): Unit = {
    if(token.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      htmlfile.write("<title>")
      getNextToken()

      if (isValidText(token)) {
        htmlfile.write(token)
        getNextToken()

        if (token.equals(CONSTANTS.BRACKETE.toString)) {
          htmlfile.write("</title>\n")
          getNextToken()
        }
      }
    }
  }

  /** body()
    *
    * A method that calls the appropriate translating functions within a gittex document
    */
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

  /** innertext()
    *
    * A method that calls the appropriate translating functions depending on the token
    * and writes valid text within the body to our html file.
    */
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
      htmlfile.write(token)
      getNextToken()
      innertext()
    }
  }

  /** paragraph()
    *
    * Translates paragraphs to the following format:
    * <p> paragraph contents </p>
    */
  def paragraph(): Unit = {
    if(token.equalsIgnoreCase(CONSTANTS.PARAB)) {
      htmlfile.write("<p>\n")
      getNextToken()

      if(token.equals(CONSTANTS.DEFB)) {
        variableDefine()
      }

      if(!token.equals(CONSTANTS.PARAE)) {
        innertext()
      }

      if(token.equals(CONSTANTS.PARAE)) {
        htmlfile.write("\n</p>\n")
        getNextToken()
      }
    }
  }

  /** heading()
    *
    * Translates headings to the following format:
    * <h1> heading text </h1>
    */
  def heading(): Unit = {
    if(token.equals(CONSTANTS.HEADING.toString)) {
      htmlfile.write("<h1>")
      getNextToken()

      if(isValidText(token)){
        htmlfile.write(token.toList.filter(_ != '\n').mkString)
        htmlfile.write("</h1>\n")
        getNextToken()
      }
    }
  }

  /** variable() creates a variable
    *
    * Stores variables defined within the grammar to a Map
    */
  def variableDefine(): Unit = {
    var varName: String = ""

    if (token.equalsIgnoreCase(CONSTANTS.DEFB)) {
      getNextToken()

      if(isValidText(token)) {
        varName = token.toList.filter(!_.isWhitespace).mkString
        getNextToken()

        if(token.equals(CONSTANTS.EQSIGN.toString)) {
          getNextToken()

          if(isValidText(token)) {
            // checks to see if variable has already been declared
            // if so, it stores the variable in separate Map to simulate local scoping
            if(gVars.contains(varName)) {
              pVars += (varName -> token)
            }
            else {
              gVars += (varName -> token)
            }

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

  /** variableUse() outputs the value of a defined variable
    *
    * Pulls variable values from the appropriate Map.
    * Spits out an error if an undefined variableis called.
    */
  def variableUse(): Unit = {
    if(token.equalsIgnoreCase(CONSTANTS.USEB)) {
      getNextToken()

      if(isValidText(token)) {
        var key = token.toList.filter(!_.isWhitespace).mkString

        // Checks to see if the variable we want to use is within the 'local scope'
        // then checks to see if its in the 'global scope' and pulls the appropriate value
        if(pVars.contains(key)) {
          htmlfile.write(pVars(key) + ' ')
          pVars -= key
        }
        else if(gVars.contains(key)) {
          htmlfile.write(gVars(key) + ' ')
        }
        else {
          println("Static semantic error: The variable you are trying to use, " + token + ", has not been defined.")
          System.exit(1)
        }
        getNextToken()

        if(token.equals(CONSTANTS.BRACKETE.toString)) {
          getNextToken()
        }
      }
    }
  }

  /** bold()
    *
    * Translates bold to the following format:
    * <b> bold text </b>
    */
  def bold(): Unit = {
    if(token.equals(CONSTANTS.BOLD.toString)) {
      htmlfile.write("<b>")
      getNextToken()

      if(isValidText(token)) {
        htmlfile.write(token)
        getNextToken()

        if(token.equals(CONSTANTS.BOLD.toString)) {
          htmlfile.write("</b>")
          getNextToken()
        }
      }
    }
  }

  /** list
    *
    * Translates list items to the following format:
    * <li> item contents </li>
    */
  def listItem(): Unit = {
    if (token.equals(CONSTANTS.LISTITEM.toString)) {
      htmlfile.write("<li>")
      getNextToken()

      inneritem()
      htmlfile.write("</li>\n")

      if(token.equals(CONSTANTS.LISTITEM.toString))
        listItem()
    }
  }

  /** inneritem()
    *
    * A method that calls the appropriate translating functions depending on the token
    * and writes valid text within a list tag to our html file.
    */
  def inneritem(): Unit = {
    if(isValidText(token)) {
      htmlfile.write(token.toList.filter(_ != '\n').mkString)
      getNextToken()
    }
    if(token.equals(CONSTANTS.USEB)) {
      variableUse()
    }
    if(token.equals(CONSTANTS.BOLD.toString)) {
      bold()
    }
    if(token.equals(CONSTANTS.LINKB.toString)) {
      link()
    }

    if(!token.equals("\n"))
      inneritem()
    else
      getNextToken()
  }

  /** link()
    *
    * Translates links to the following format:
    * <a href="url"> linked text </a>
    */
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
              htmlfile.write("<a href=\"" + token + "\"> " + lname + " </a>")
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

  /** image()
    *
    * Translates images to the following format:
    * <img src="image-url" alt="image label">
    */
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
              htmlfile.write("<img src =\"" + token + "\" alt=\"" + lname + "\">")
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

  /** newline()
    *
    * Translates a newline to <br>
    */
  def newline(): Unit = {
    if(token.equals(CONSTANTS.NEWLINE.toString)){
      htmlfile.write("<br>\n")
      getNextToken()
    }
  }

  /** isValidText()
    *
    * Checks to see if the string is valid by checking to see if each character in the string
    * is contained in the list of valid characters.
    */
  def isValidText(candidate: String): Boolean = {
    candidate.toList.forall(x => CONSTANTS.validText.contains(x))
  }
}
