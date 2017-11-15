package edu.towson.cs.cosc455.oagunl1.project1

/**
  * Created by Wale on 10/11/17.
  */
object CONSTANTS {
  val DOCB: String = "\\BEGIN"
  val DOCE: String = "\\END"
  val TITLEB: String = "\\TITLE["
  val BRACKETE: String = "]"
  val HEADING: String = "#"
  val PARAB: String = "\\PARAB"
  val PARAE: String = "\\PARAE"
  val BOLD: String = "*"
  val LISTITEM: String = "+"
  val NEWLINE: String = "\\\\"
  val LINKB: String = "["
  val ADDRESSB: String = "("
  val ADDRESSE: String = ")"
  val IMAGEB: String = "!["
  val DEFB: String = "\\DEF["
  val EQSIGN: String = "="
  val USEB: String = "\\USE["
  val REQTEXT: String = ",.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  //val TEXT: String = REQTEXT
  val TOKENS: String = "\\#*[!]+()="

}

/*
  /**
    * Checks if the provided text is valid
    *
    * @return true if text exists in the TEXT array, else return false
    */

  def validText (text : String) : Boolean = {

      if(text.contains(TEXT)) {
        return true
      }
      else return false

  }
}
*/