package cosc455.project1

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var tstack = Stack[String]()
  var variableNames = new ListBuffer[String]()

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      tstack.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
        variableDefine()
      }

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
        title()
        body()

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
          tstack.push(Compiler.currentToken)
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

      while (! Compiler.currentToken.equals(CONSTANTS.BRACKETE)) {
        // accept tokens,  then use validText to check maybe via "contains" function
        if(CONSTANTS.validText.contains(Compiler.currentToken)) {
          // add to stack
          Compiler.Scanner.getNextToken()
        }
        else {
          println("Error: variable name not valid")
        }
        Compiler.Scanner.getNextToken()
      }
    }
    else {
      println("Error: Missing \"\\TITLE[\".")
    }
  }

  override def body(): Unit = ???

  override def paragraph(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Unit = ???

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      // add to stack
      Compiler.Scanner.getNextToken()

      while (! Compiler.currentToken.equals(CONSTANTS.BRACKETE)) {
        // accept tokens, put them in a list then use validText to check maybe via "contains" function
        if(CONSTANTS.validText.contains(Compiler.currentToken)) {
          // add to stack
          // add to the list of variable names (needed later)
          // variableNames += Compiler.currentToken.mkString
          Compiler.Scanner.getNextToken()
        }
        else {
          println("Error: variable name not valid")
        }
      }

      Compiler.Scanner.getNextToken()
    }
    else {
      println("Error: Variable use must start with \"\\\\USE[\"")
    }
  }

  override def bold(): Unit = ???

  override def listItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???
}
