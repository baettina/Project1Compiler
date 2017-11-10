import cosc455.project1.CONSTANTS
import scala.collection.mutable.ListBuffer

var tokenlist = List("\\begin", "\\title[", "the simpsons", "]", "#", "The simpsons",
"\\parab", "the members of the", "[", "the simpsons", "]", "(", "https://en.wikipedia.org/wiki/The_Simpsons",
")", "are:", "\\parae", "+", "homer simpson", "+", "Marge Simpson", "+", "Bart Simpson",
"+", "Lisa Simpson", "+", "Maggie Simpson Here is a picture:", "\\\\", "![", "The Simpsons", "]", "(",
"https://upload.wikimedia.org/wikipedia/en/0/0d/Simpsons_FamilyPicture.png", ")", "\\end")
var html = ListBuffer[String]()
var position = 0

tokenlist(0)


def convert(): Unit = {
  if(tokenlist(position).equalsIgnoreCase(CONSTANTS.DOCB)) {
    cBeg()
  }
  else if(tokenlist(position).equalsIgnoreCase(CONSTANTS.TITLEB)) {
    cTitle()
  }
  else if(tokenlist(position).equals(CONSTANTS.HEADING.toString)) {
    cHeading()
  }
  else if(tokenlist(position).equalsIgnoreCase(CONSTANTS.PARAB)) {
    cPBeg()
  }
  else if(tokenlist(position).equals(CONSTANTS.LINKB.toString)) {
    cLink()
  }
  else{
    text()
  }
}

def cBeg(): Unit = {
  html += "<html>\n"
  position += 1
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
  html += tokenlist(position)
  position += 1
  html += "</h1>\n"
}

def cPBeg(): Unit = {
  html += "<p>"
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
  println(tokenlist(position))
}

def startConv(): Unit = {
  while(position < 20){
    convert()
  }
}

startConv()

html.toList.foreach(println(_))