package cosc455.project1

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var tstack = Stack[String]()
  //var variables = Map[String, ListBuffer[String]]()
  var variableNames = new ListBuffer[String]()
  //var varnodes = Map.empty[String, List[String]]

  /** addChar() adds current char to the token we are building
    *
    * The current char is appended to the List,
    * the length of the token is incremented by 1
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
          if(Compiler.Scanner.position + 2 < Compiler.fileContents.length)
            println("Error: Everything must be within the document block")
          else{
            println("WOO HOO!! proper syntax")
            Compiler.gittexTokens = tstack.toList.reverse
          }
        }
        else {
          println("Error: Missing \"\\END\".  This is required at the end of every gittex.\n")
          System.exit(1)
        }
      }
      else {
        println("Error: Must specify title before body begins.\n")
        System.exit(1)
      }
    }
    else {
      println("Error: Missing \"\\BEGIN\". This is required at the beginning of every gittex.")
      System.exit(1)
    }
  }


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
          println("Error: Title annotation must end with ']'.")
          System.exit(1)
        }
      }
      else {
        println("Error: Missing title or title must not contain illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Error: Missing \"\\TITLE[\".")
      System.exit(1)
    }
  }

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

  override def heading(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.HEADING.toString)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(isValidText(Compiler.currentToken)){
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Error: Can't have an empty heading or header must not contain illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Error: Headings must start with '#'.")
      System.exit(1)
    }
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      // check for REQTEXT
      if(isValidText(Compiler.currentToken)) {
        // the token should be variable name
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        // push the = sign
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
              println("Error: Variable definition must end with ']'.")
              System.exit(1)
            }
          }
          else{
            println("Error: Missing variable value or variable value must not contain illegal characters.")
            System.exit(1)
          }
        }
        else {
          println("Error: Missing '=' sign.")
          System.exit(1)
        }
      }
      else {
        println("Error: Missing variable name or variable name must not contain illegal characters.")
        System.exit(1)
      }
    }
  }

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      // next token should be the variable name - check this in the semantic analyzer
      if(isValidText(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE.toString)) {
          // add ] to stack
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else {
          println("Error: Incorrect syntax for variable use. Must end with ]")
          System.exit(1)
        }
      }
      else {
        println("Error: Missing variable name or variable name must not contain special characters.")
        System.exit(1)
      }
    }
    else {
      println("Error: Variable use must start with \"\\\\USE[\"")
      System.exit(1)
    }
  }

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
          println("Error: Bold must end with '*'.")
          System.exit(1)
        }
      }
      else {
        println("Error: Bold text contains illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Error: Bold must begin with with '*'.")
      System.exit(1)
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.LISTITEM.toString)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      inneritem()
      listItem()
    }
  }

  def inneritem(): Unit = {
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
    if(isValidText(Compiler.currentToken)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //inneritem()
    }

  }

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
                println("Error: Link mush end with ']'")
                System.exit(1)
              }
            }
            else {
              println("Error: Link url contains invalid characters.")
              System.exit(1)
            }
          }
          else {
            println("Error: Link is missing a '('.")
            System.exit(1)
          }
        }
        else {
          println("Error: Link is missing a ']'.")
          System.exit(1)
        }
      }
      else {
        println("Error: Link title contains illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Error: Links must begin with '['.")
      System.exit(1)
    }
  }

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
                println("Error: Images must end with ']'")
                System.exit(1)
              }
            }
            else {
              println("Error: Image url contains invalid characters.")
              System.exit(1)
            }
          }
          else {
            println("Error: Image link is missing a '('.")
            System.exit(1)
          }
        }
        else {
          println("Error: Image link is missing a ']'.")
          System.exit(1)
        }
      }
      else {
        println("Error: Image link title contains illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Error: Images must begin with '!['.")
      System.exit(1)
    }
  }

  override def newline(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.NEWLINE.toString)){
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  def isValidText(candidate: String): Boolean = {
    candidate.toList.forall(x => CONSTANTS.validText.contains(x))
  }
}