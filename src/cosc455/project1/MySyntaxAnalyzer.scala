package cosc455.project1

import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var tstack = Stack[String]()
  var variables = Map[String, String]()
  var variableNames = new ListBuffer[String]()

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
          println("WOO HOO!! proper syntax")
          // file has proper syntax
        }
        else {
          println("Error: Missing \"\\DOCE\".  This is required at the end of every gittex.\n")
          System.exit(1)
        }
      }
      else {
        println("Error: Must specify title before body begins.\n")
        System.exit(1)
      }
    }
    else {
      println("Error: Missing \"\\DOCB\". This is required at the beginning of every gittex.")
      System.exit(1)
    }
  }

  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(CONSTANTS.validText.contains(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE)) {
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
    if(Compiler.currentToken.equals(CONSTANTS.USEB) || Compiler.currentToken.equals(CONSTANTS.HEADING) ||
       Compiler.currentToken.equals(CONSTANTS.BOLD) || Compiler.currentToken.equals(CONSTANTS.LISTITEM) ||
       Compiler.currentToken.equals(CONSTANTS.IMAGEB) || Compiler.currentToken.equals(CONSTANTS.LINKB) ||
       CONSTANTS.validText.contains(Compiler.currentToken)) {
      innertext()
      body()
    }

    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph()
      body()
    }

    if(Compiler.currentToken.equals(CONSTANTS.NEWLINE)) {
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
    if(Compiler.currentToken.equals(CONSTANTS.HEADING)) {
      heading()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.BOLD)) {
      bold()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.LISTITEM)) {
      listItem()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.IMAGEB)) {
      image()
      innertext()
    }
    if(Compiler.currentToken.equals(CONSTANTS.LINKB)) {
      link()
      innertext()
    }
    if(CONSTANTS.validText.contains(Compiler.currentToken)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innertext()
    }
  }

  override def heading(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.HEADING)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(CONSTANTS.validText.contains(Compiler.currentToken)){
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
    var varName: String = ""
    var varVal: String = ""

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      // check for REQTEXT
      if(CONSTANTS.validText.contains(Compiler.currentToken)) {
        // the token should be variable name
        tstack.push(Compiler.currentToken)
        varName = Compiler.currentToken
        Compiler.Scanner.getNextToken()

        // push the = sign
        if(Compiler.currentToken.equals(CONSTANTS.EQSIGN)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()

          if(CONSTANTS.validText.contains(Compiler.currentToken)) {
            tstack.push(Compiler.currentToken)
            varVal = Compiler.currentToken
            Compiler.Scanner.getNextToken()

            if(Compiler.currentToken.equals(CONSTANTS.BRACKETE)) {
              tstack.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              // syntax is correct, add information to the map of variables
              variables += (varName.toString -> varVal.toString)

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
    else {
      println("Error: Variable definitions must begin with '['.")
      System.exit(1)
    }
  }

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      // next token should be the variable name - check this in the semantic analyzer
      if(CONSTANTS.validText.contains(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE)) {
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
    if(Compiler.currentToken.equals(CONSTANTS.BOLD)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(CONSTANTS.validText.contains(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BOLD)) {
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
    if (Compiler.currentToken.equals(CONSTANTS.LISTITEM)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if (CONSTANTS.validText.contains(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Error: List item contains illegal characters.")
        System.exit(1)
      }
    }
    else {
      println("Error: List items must begin with '+'.")
      System.exit(1)
    }
  }

  override def link(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.LINKB)) {
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if(CONSTANTS.validText.contains(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()

          if(Compiler.currentToken.equals(CONSTANTS.ADDRESSB)) {
            tstack.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

            if(CONSTANTS.validText.contains(Compiler.currentToken)) {
              tstack.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()

              if(Compiler.currentToken.equals(CONSTANTS.ADDRESSE)) {
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

      if(CONSTANTS.validText.contains(Compiler.currentToken)) {
        tstack.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equals(CONSTANTS.BRACKETE)) {
          tstack.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()

          if(Compiler.currentToken.equals(CONSTANTS.ADDRESSB)) {
            tstack.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

            if(CONSTANTS.validText.contains(Compiler.currentToken)) {
              tstack.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()

              if(Compiler.currentToken.equals(CONSTANTS.ADDRESSE)) {
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
    if(Compiler.currentToken.equals(CONSTANTS.NEWLINE)){
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }
}