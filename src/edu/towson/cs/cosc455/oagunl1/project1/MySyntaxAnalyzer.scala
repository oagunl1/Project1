package edu.towson.cs.cosc455.oagunl1.project1

/**
  * Created by Wale on 10/11/17.
  */


import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var parse = Stack[String]()
  var found: Boolean = false;

  override def gittex(): Unit = {
    docb()
    while (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      variableDefine()
    }
    title()
    body()
    doce()
  }

  override def title(): Unit = {
    titleb()
    reqText()
    brackete()
  }

  override def body(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      variableDefine()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {

    }
    else {
      innerText()
      body()
    }
  }

  override def paragraph(): Unit = {
    parab()
    variableDefine()
    while (!Compiler.currentToken.equals(CONSTANTS.DOCE) && !Compiler.currentToken.equals(CONSTANTS.PARAE)) {
      innerText()
    }
    parae()
  }


  def innerText(): Unit = {

    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      if(parse.contains(CONSTANTS.PARAB)){
        parae()
      }
      else {
        println("Syntax Error: \\PARAB is yet to be defined")
        System.exit(4)
      }
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      return
    }
    else {
      if (Compiler.Scanner.checkText(Compiler.currentToken)) {
        parse.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
    }

  }


  override def heading(): Unit = {
    head()
    reqText()
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      defb()
      reqText()
      eqsign()
      reqText()
      brackete()
    }
  }

  override def variableUse(): Unit = {
    useb()
    reqText()
    brackete()
  }

  override def bold(): Unit = {
    bold()
    reqText()
    bold()
  }

  def listItem(): Unit = {
    listItemb()
    innerItem()
  }

  def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerItem()
    }

    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      found = true
    }
    else if (Compiler.Scanner.checkText(Compiler.currentToken)) {
      reqText()
      innerItem()
    }
    else
      found = true
  }

  override def link(): Unit = {
    linkb()
    reqText()
    brackete()
    addressb()
    reqText()
    addresse()
  }

  override def image(): Unit = {
    imageb()
    reqText()
    brackete()
    addressb()
    reqText()
    addresse()
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parse.push(CONSTANTS.NEWLINE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: \\\\ was expected instead of " + Compiler.currentToken)
      System.exit(5)
    }
  }

  def docb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parse.push(CONSTANTS.DOCB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: \\BEGIN was expected instead of " + Compiler.currentToken)
      System.exit(6)
    }
  }

  def doce(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      parse.push(CONSTANTS.DOCE)
      Compiler.Scanner.getNextToken()
      /*if (Compiler.Scanner.nextChar.equals('\n') || Compiler.Scanner.nextChar.equals('\t') || Compiler.Scanner.nextChar.equals(' '))
        return
      else {
        println("SYNTAX ERROR: items after document end statement")
        System.exit(7)
      }*/ return
    }
    else {
      println("Syntax Error: \\END was expected instead of " + Compiler.currentToken)
      System.exit(8)
    }
  }

  def titleb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      parse.push(CONSTANTS.TITLEB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: \\TITLE[ was expected instead of " + Compiler.currentToken)
      System.exit(9)
    }
  }

  def reqText(): Unit = {
    if (Compiler.Scanner.checkText(Compiler.currentToken)) {
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: Text was expected instead of " + Compiler.currentToken)
      System.exit(10)
    }
  }

  def brackete(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("]")) {
      parse.push("]")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: ] was expected at the end instead of " + Compiler.currentToken)
      System.exit(11)
    }
  }

  def parab(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parse.push(CONSTANTS.PARAB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: \\PARAB was expected instead of " + Compiler.currentToken)
      System.exit(12)
    }
  }

  def parae(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      parse.push(CONSTANTS.PARAE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: \\PARAE was expected instead of " + Compiler.currentToken)
      System.exit(13)
    }
  }

  def head(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parse.push(CONSTANTS.HEADING)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: # was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def defb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parse.push(CONSTANTS.DEFB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: \\DEF[ was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def eqsign(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.EQSIGN)) {
      parse.push("=")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: = was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def useb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parse.push(CONSTANTS.USEB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: \\USE[ was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def boldb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parse.push(CONSTANTS.BOLD)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: * was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def listItemb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parse.push(CONSTANTS.LISTITEM)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: + was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def linkb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("[")) {
      parse.push("[")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: [ was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def addressb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
      parse.push("(")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: ( was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def addresse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(")")) {
      parse.push(")")
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: ) was expected instead of " + Compiler.currentToken)
      System.exit(1)
    }
  }

  def imageb(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parse.push(CONSTANTS.IMAGEB)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Syntax Error: ![ was expected instead instead " + Compiler.currentToken)
      System.exit(1)
    }
  }
}
