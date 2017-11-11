package cosc455.project1

import scala.collection.mutable.Stack

/**
  * Checks to see if file conforms to our grammar
  */
class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var tstack = Stack[String]()

  /** gittex() is the start state of our grammar
    *
    * Implements the grammar:
    * DOCB <variable-define> <title> <body> DOCE
    * and outputs appropriate error messages
    */
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
        title()
        body()

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
          tstack.push(Compiler.currentToken)

          // Checks to see if we the document end tag occurred at the end of the document or prior.
          // If it appeared prior, we can assume that the rest of the tokens are illegal according to our grammar.
          if(Compiler.Scanner.position + 2 < Compiler.fileContents.length){
            println("Syntax error: Everything must be within the document block")
            System.exit(1)
            }
          else{
            Compiler.gittexTokens = tstack.toList.reverse
          }
        }
        else {
          println("Syntax error: Missing \"\\END\".  This is required at the end of every gittex.\n")
          System.exit(1)
        }
      }
      else {
        println("Syntax error: Must specify title before body begins.\n")
        System.exit(1)
      }
    }
    else {
      println("Syntax error: Missing \"\\BEGIN\". This is required at the beginning of every gittex.")
      System.exit(1)
    }
  }

  /** title()
    *
    * Implements the grammar:
    * TITLEB REQTEXT BRACKETE
    * and outputs appropriate error messages
    */
  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE.toString)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else {
          println("Syntax error: Title annotation must end with ']'.")
          System.exit(1)
        }
      }
      else {
        println("Syntax error: Missing title or title must not contain illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Syntax error: Missing \"\\TITLE[\".")
      System.exit(1)
    }
  }

  /** title()
    *
    * Implements the grammar:
    * <inner-text> <body> | <paragraph> <body> | <newline> <body> | ε
    * and outputs appropriate error messages
    */
  override def body(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.USEB) || Compiler.currentToken.equals(CONSTANTS.HEADING.toString) ||
       Compiler.currentToken.equals(CONSTANTS.BOLD.toString) || Compiler.currentToken.equals(CONSTANTS.LISTITEM.toString) ||
       Compiler.currentToken.equals(CONSTANTS.IMAGEB.toString) || Compiler.currentToken.equals(CONSTANTS.LINKB.toString) ||
       isValidText(Compiler.currentToken)) {
      innertext()
      body()
    }

    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph()
      body()
    }

    if(Compiler.currentToken.equals(CONSTANTS.NEWLINE.toString)) {
      newline()
      body()
    }
  }

  /** paragraph()
    *
    * Implements the grammar:
    * PARAB <variable-define> <inner-text> PARAE
    * and outputs appropriate error messages
    */
  override def paragraph(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(Compiler.currentToken.equals(CONSTANTS.DEFB)) {
        variableDefine()
      }

      if(!Compiler.currentToken.equals(CONSTANTS.PARAE)) {
        innertext()
      }

      if(Compiler.currentToken.equals(CONSTANTS.PARAE)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Error: Paragraphs must end with the tag \"\\PARAE\"")
        System.exit(1)
      }
    }
    else{
      println("Error: Paragraphs must begin with the tag \"\\PARAB\"")
      System.exit(1)
    }
  }

  /** innertext()
    *
    * Implements the grammar:
    * <variable-use> <inner-text> | <heading> <inner-text> | <bold> <inner-text> | <listitem> <inner-text> |
    * <image> <inner-text> | <link> <inner-text> | TEXT <inner-text> | ε
    *  and outputs appropriate error messages
    */
  def innertext(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.USEB)) {
      variableUse()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.HEADING.toString)) {
      heading()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.BOLD.toString)) {
      bold()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.LISTITEM.toString)) {
      listItem()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.IMAGEB)) {
      image()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.LINKB.toString)) {
      link()
      innertext()
    }
    // Checks to see if the paragraph end was used prior ot the paragraph begin and outputs an error.
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE) && !tstack.toList.contains(CONSTANTS.PARAB)){
      println("Syntax error: Closing tag \"\\PARAE\" was called without the opening tag \"\\PARAB\"")
      System.exit(1)
    }
    if(isValidText(Compiler.currentToken)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innertext()
    }
  }

  /** heading()
    *
    * Implements the grammar:
    * HEADING REQTEXT | ε
    * and outputs appropriate error messages
    */
  override def heading(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.HEADING.toString)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)){
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax error: Can't have an empty heading or header must not contain illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Syntax error: Headings must start with '#'.")
      System.exit(1)
    }
  }

  /** variableDefine()
    *
    * Implements the grammar:
    * DEFB REQTEXT EQSIGN REQTEXT BRACKETE <variable-define> | ε
    * and outputs appropriate error messages
    */
  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.EQSIGN.toString)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()

          if(isValidText(Compiler.currentToken)) {
            tstack.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

            if(Compiler.currentToken.equals(CONSTANTS.BRACKETE.toString)) {
              tstack.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()

              variableDefine()
            }
            else {
              println("Syntax error: Variable definition must end with ']'.")
              System.exit(1)
            }
          }
          else{
            println("Syntax error: Missing variable value or variable value must not contain illegal characters.")
            System.exit(1)
          }
        }
        else {
          println("Syntax error: Missing '=' sign.")
          System.exit(1)
        }
      }
      else {
        println("Syntax error: Missing variable name or variable name must not contain illegal characters.")
        System.exit(1)
      }
    }
  }

  /** variableUse()
    *
    * Implements the grammar:
    * USEB REQTEXT BRACKETE | ε
    * and outputs appropriate error messages
    */
  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE.toString)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else {
          println("Syntax error: Incorrect syntax for variable use. Must end with ]")
          System.exit(1)
        }
      }
      else {
        println("Syntax error: Missing variable name or variable name must not contain special characters.")
        System.exit(1)
      }
    }
    else {
      println("Syntax error: Variable use must start with \"\\\\USE[\"")
      System.exit(1)
    }
  }

  /** variableUse()
    *
    * Implements the grammar:
    * BOLD TEXT BOLD | ε
    * and outputs appropriate error messages
    */
  override def bold(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.BOLD.toString)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BOLD.toString)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else {
          println("Syntax error: Bold must end with '*'.")
          System.exit(1)
        }
      }
      else {
        println("Syntax error: Bold text contains illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Syntax error: Bold must begin with with '*'.")
      System.exit(1)
    }
  }

  /** listItem()
    *
    * Implements the grammar:
    * LISTITEMB <inner-item> <list-item> | ε
    * and outputs appropriate error messages
    */
  override def listItem(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.LISTITEM.toString)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      inneritem()
      tstack.push("\n")

      if(Compiler.currentToken.equals(CONSTANTS.LISTITEM.toString))
        listItem()
    }
  }

  /** inneritem()
    *
    * Implements the grammar:
    * <variable-use> <inner- item> | <bold> <inner- item> |
    * <link> <inner- item> | REQTEXT <inner- item> | ε
    * and outputs appropriate error messages
    */
  def inneritem(): Unit = {
    if(isValidText(Compiler.currentToken)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    if(Compiler.currentToken.equals(CONSTANTS.USEB)) {
      variableUse()
      inneritem()
    }
    if(Compiler.currentToken.equals(CONSTANTS.BOLD.toString)) {
      bold()
      inneritem()
    }
    if(Compiler.currentToken.equals(CONSTANTS.LINKB.toString)) {
      link()
      inneritem()
    }
  }

  /** link()
    *
    * Implements the grammar:
    * LINKB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
    * and outputs appropriate error messages
    */
  override def link(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.LINKB.toString)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE.toString)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()

          if(Compiler.currentToken.equals(CONSTANTS.ADDRESSB.toString)) {
            tstack.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

            if(isValidText(Compiler.currentToken)) {
              tstack.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()

              if(Compiler.currentToken.equals(CONSTANTS.ADDRESSE.toString)) {
                tstack.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
              else {
                println("Syntax error: Link mush end with ']'")
                System.exit(1)
              }
            }
            else {
              println("Syntax error: Link url contains invalid characters.")
              System.exit(1)
            }
          }
          else {
            println("Syntax error: Link is missing a '('.")
            System.exit(1)
          }
        }
        else {
          println("Syntax error: Link is missing a ']'.")
          System.exit(1)
        }
      }
      else {
        println("Syntax error: Link title contains illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Syntax error: Links must begin with '['.")
      System.exit(1)
    }
  }

  /** image()
    *
    * Implements the grammar:
    * IMAGEB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
    * and outputs appropriate error messages
    */
  override def image(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.IMAGEB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE.toString)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()

          if(Compiler.currentToken.equals(CONSTANTS.ADDRESSB.toString)) {
            tstack.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

            if(isValidText(Compiler.currentToken)) {
              tstack.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()

              if(Compiler.currentToken.equals(CONSTANTS.ADDRESSE.toString)) {
                tstack.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
              else {
                println("Syntax error: Images must end with ']'")
                System.exit(1)
              }
            }
            else {
              println("Syntax error: Image url contains invalid characters.")
              System.exit(1)
            }
          }
          else {
            println("Syntax error: Image link is missing a '('.")
            System.exit(1)
          }
        }
        else {
          println("Syntax error: Image link is missing a ']'.")
          System.exit(1)
        }
      }
      else {
        println("Syntax error: Image link title contains illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Syntax error: Images must begin with '!['.")
      System.exit(1)
    }
  }

  /** newline()
    *
    * Implements the grammar:
    * NEWLINE | ε
    * and outputs appropriate error messages
    */
  override def newline(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.NEWLINE.toString)){
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
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