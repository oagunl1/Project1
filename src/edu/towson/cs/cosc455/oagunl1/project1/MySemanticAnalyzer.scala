package edu.towson.cs.cosc455.oagunl1.project1

/**
  * Created by Wale on 10/11/17.
  */

import java.io._
import scala.collection.mutable.Stack
import java.io.{PrintWriter, FileWriter}

class MySemanticAnalyzer {

  var outputStack = Stack[String]() // outStack
  var parse = Stack[String]()
  var variablenameArray = new Array[String](10)
  var valueArray = new Array[String](10)
  var nextToken: String = ""
  var output: String = ""
  var counter: Int = 0

  def Semantics(): Unit = {
    parse = Compiler.Parser.parse.reverse
    nextToken = parse.pop()

    begin()
  }

  def begin(): Unit = {
    while (!parse.isEmpty) {
      if (nextToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
        outputStack.push("<html>\n\t")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        outputStack.push("<head>\n\t\t")
        outputStack.push("<title>")
        outputStack.push(parse.pop())
        outputStack.push("</title>\n\t")
        outputStack.push("</head>\n\t")
        parse.pop()
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
        outputStack.push("<h1> ")
        outputStack.push(parse.pop())
        outputStack.push("</h1>\n")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
        outputStack.push("<p> ")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        outputStack.push("</p>\n")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        outputStack.push("<b>")
        outputStack.push(parse.pop())
        outputStack.push("</b>")
        parse.pop()
        nextToken = parse.pop()
      }

      else if (nextToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        outputStack.push("\t\t\t<li>")
        nextToken = parse.pop()
        if (nextToken.contains("\n") && !parse.isEmpty && !nextToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
          outputStack.push(nextToken)
        }
        else {
          if(!nextToken.equalsIgnoreCase(CONSTANTS.DOCE))
            begin()
        }
        outputStack.push("\t\t\t</li>\n")
        if(!parse.isEmpty)
          nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        outputStack.push("\t\t\t\t<br>")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
        val temp = parse.pop()
        parse.pop()
        parse.pop()
        nextToken = parse.pop()
        parse.pop()

        outputStack.push("<a href = \"")
        outputStack.push(nextToken)
        outputStack.push("\">")
        outputStack.push(temp)
        outputStack.push("</a> ")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
        val temp = parse.pop()
        parse.pop()
        parse.pop()
        nextToken = parse.pop()
        parse.pop()

        outputStack.push("<img src =\"")
        outputStack.push(nextToken)
        outputStack.push("\" alt=\"")
        outputStack.push(temp)
        outputStack.push("\">")
        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        var variableName = parse.pop()
        parse.pop()
        val value = parse.pop()
        parse.pop()
        variableName = variableName.filter(!" ".contains(_))
        val index = variablenameArray.indexOf(variableName)
        if (index != -1) {
          variablenameArray(index) = variableName
          valueArray(index) = value
        }
        else {
          variablenameArray(counter) = variableName
          valueArray(counter) = value
          counter += 1
        }

        nextToken = parse.pop()
      }
      else if (nextToken.equalsIgnoreCase(CONSTANTS.USEB)) {
        var variableName: String = " " + parse.pop()
        parse.pop()
        variableName = variableName.filter(!" ".contains(_))
        if(variablenameArray.contains(variableName))
          outputStack.push(valueArray(variablenameArray.indexOf(variableName)) + " ")
        else {
          println("Static Semantic Error:" + variableName + " is yet to be defined defined")
          System.exit(3)
        }

        nextToken = parse.pop()
      }

      else if (nextToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        outputStack.push("</html>")
      }
      else {
        outputStack.push(nextToken)
        nextToken = parse.pop()
      }
    }


    val output = outputStack.reverse.mkString
    val print = new PrintWriter((new File(Compiler.filename)))
    print.write(output)
    print.close

    def changeExtension(name : String): String = {
      //output file
      val output = name take(name.length - 4)
      output + ".html"
    }

  }
}
