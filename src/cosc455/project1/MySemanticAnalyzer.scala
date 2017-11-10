package cosc455.project1

import scala.collection.mutable.Stack

class MySemanticAnalyzer {

  var htmlStack = Stack[String]()

  def start(gtokens: List[String]): Unit =  {

    gtokens.foreach(println(_))

  }
}
