package edu.towson.cs.cosc455.oagunl1.project1

/**
  * Created by Wale on 10/11/17.
  */

import java.awt.Desktop
import java.io.{File, FileWriter, IOException, PrintWriter}
import scala.io.Source

object Compiler {

  // currentToken variable
  var currentToken : String = ""
  var fileText : String = ""
  var filename : String = ""
  var theEnd : Boolean = false

  // Initialize classes and vars
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val Semantic = new MySemanticAnalyzer



  def main(args: Array[String]) = {

      // get input file name from command line argument
      filename = args(0)

      // check to make sure its the right extension
      checkExtension(filename)

      val file = Source.fromFile(filename)
      fileText = try file.mkString finally file.close()

      // sends file to lexical analyzer
      Scanner.start(fileText)

      // loops through file and gets the next token till file is empty
      while (Scanner.position < Scanner.sizeofFile && !theEnd) {
        Scanner.getNextToken()

        Parser.gittex()

        if (currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
           theEnd = true
        }
      }

        filename = changeExtension(filename)

        Semantic.Semantics()

        openHTMLFileInBrowser(filename)


  }

  /* * function to check file type extension. */
  def checkExtension(filename : String) = {
    if(!filename.endsWith(".gtx")){
      println("Failed to open " + filename + ". Incorrect file type, only .gtx file type accepted")
      System.exit(1)
    }
  }

  /* * function to change extension to .html */
  def changeExtension(name : String) = {
    //output file
    val output = name take(name.length - 4)
    "%s.html".format(output)
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browser. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }


}

//val pWriter = new PrintWriter((new FileWriter(changeExtension(filename), true)))

// for each line read from file, scan and parse
/* for (line <- Source.fromFile(filename).getLines()) {

 }*/