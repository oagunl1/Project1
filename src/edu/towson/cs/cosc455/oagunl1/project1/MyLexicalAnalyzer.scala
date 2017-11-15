package edu.towson.cs.cosc455.oagunl1.project1

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Wale on 10/11/17.
  */
class MyLexicalAnalyzer extends LexicalAnalyzer {

  //Declare values
  val lexems = new Array[String](20)
  var token = new ArrayBuffer[Char](50)
  var sourceFile: Array[Char] = Array()
  var nextChar: Char = ' '
  var position: Int = -1
  var sizeofFile: Int = 0


  /** The main driver of the lexical analyzer, it initializes the lexems and
    * reads the source file character by character-by-character */
  def start (file: String) : Unit = {
    InitializeLexems()
    sourceFile = file.toCharArray
    sizeofFile = sourceFile.length - 1

  }

  /**
	This method adds the current character to the token
	*/

  override def addChar(): Unit = {
    token += nextChar
  }

  /** This method gets the next character from the "program" string.*/
  override def getChar() : Unit = {
    if (position < sizeofFile){
      position += 1
      nextChar = sourceFile.charAt(position)
    }
    else
      return
  }

  /*
   * This method does a character-by-character analysis to get the next token
   */
  override def getNextToken(): Unit = {

    getChar()
    specialChar()


    if (nextChar.equals('+') || nextChar.equals('=') || nextChar.equals('#') || nextChar.equals('(') || nextChar.equals(')') || nextChar.equals(']') || nextChar.equals('[')) {
      addChar()
    }

    else if (nextChar.equals('\\')) {
      addChar()
      getChar()
      while (!nextChar.equals('[') && nextChar != '\r' && nextChar != '\n' && nextChar != '\\') {
        if (nextChar.equals('\r')) {
          addChar()
        }
        else  {
          addChar()
          getChar()
        }
      }
      if (nextChar.equals('[')) {
        addChar()
      }
      if (nextChar.equals('\\')) {
        addChar()
      }
    }

    else if (nextChar.equals('*')) {
      addChar()
      getChar()
      storeToken()
      position -= 1

    }
    else if (nextChar.equals('!')) {
      addChar()
      getChar()
      if (nextChar.equals('[')) {
        addChar()
      }
    }
    else {
      addChar()
      getChar()
      while (!CONSTANTS.TOKENS.contains(nextChar)) {
        addChar()
        if (position < sizeofFile) {
          getChar()
        }
        else {
          return
        }
      }
      position -= 1
    }
    storeToken()
  }


  /**
    * uses the lookup method to verify tokens, then
    * stores token in the compiler class's currentToken global String variable
    * if token string is invalid, an error message is displayed and the compiler
    * exits without creating an output file
    */

  def storeToken(): Unit = {
    val candidateToken : String = token.mkString // convert tokens to a string and assigns it to candidateToken

    if (lookup(candidateToken)) { //use lookup method to validate string
      Compiler.currentToken = candidateToken // change the current token in compiler to the string of tokens
      token.clear() // clear tokens
    }

    else if (checkText(candidateToken)) {
      Compiler.currentToken = candidateToken
      token.clear()
    }
    else {
      println("Lexical Error Token was incorrect " + candidateToken + " was found")
      System.exit(1)
    }
  }

  //check for special characters i.e new line
  def specialChar(): Unit ={
    while (nextChar.equals(' ') || nextChar.equals('\n') || nextChar.equals('\t')){
      getChar()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        getChar()
        return
      }
    }
  }


  // checks text to make sure its not terminal
  def checkText (text : String) : Boolean = {
    if (text.contains("\\"))
      return false
    else
      return true
  }

  def lookup(stuff : String): Boolean = {

    if(!lexems.contains(stuff.toUpperCase)){
      return false
    }
    else return true

  }

  def InitializeLexems(): Unit ={
    lexems(0) = "\\BEGIN"
    lexems(1) = "\\END"
    lexems(2) = "\\TITLE["
    lexems(3) = "]"
    lexems(4) = "#"
    lexems(5) = "\\PARAB"
    lexems(6) = "\\PARAE"
    lexems(7) = "*"
    lexems(8) = "+"
    lexems(9) = "\\\\"
    lexems(10) = "["
    lexems(11) = "("
    lexems(12) = ")"
    lexems(13) = "!["
    lexems(14) = "\\DEF["
    lexems(15) = "="
    lexems(16) = "\\USE["
    lexems(17) = "]"
    lexems(18) = "\""
    lexems(19) = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toString()
    /*lexems(20) = ":"
    lexems(21) = "?"
    lexems(22) = ","
    lexems(23) = "."
    lexems(24) = "/"
    lexems(25) = "_"*/
  }
}


