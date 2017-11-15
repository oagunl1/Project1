val s = "text.gtx"

//s take(s.length - 4)

val output = s take(s.length - 4)

output + ".html"
"%s.html".format(output)

s.endsWith(".gtx")

if(s.contains(".gtx")){
  println("right file format")
} else println("only accept .gtx")

val REQ2TEXT = (('a' to 'z') ++ ('0' to '9'))

val REQTEXT   : String = "\":?_/,.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

REQTEXT.contains("/")

val TEXT : String = " "

TEXT.contains(" ")

//val TOKENS    : String = "\#*[!]+()="

val DOCB : String = "\\BEGIN"
val DOCE : String = 	"\\END"
val TITLEB : String = "\\TITLE["
val BRACKETE : String = "]"
val HEADING : String = "#"
val PARAB : String = 	"\\PARAB"
val PARAE : String = 	"\\PARAE"
val BOLD : String = "*"
val LISTITEM : String = "+"
val NEWLINE : String = 	"\\\\"
val LINKB : String = 	"["
val ADDRESSB : String = "("
val ADDRESSE : String = ")"
val IMAGEB : String = "!["
val DEFB : String = 	"\\DEF["
val EQSIGN : String = 	"="
val USEB : String = "\\USE["

val STUFF : String = "\""

val lexeme = List[String]()

"wale" :: "is" :: "testing" :: lexeme

"String lists" :: lexeme

import java.util.ArrayList

val list = new ArrayList[String]

list.add("Milk")
list add "Sugar"

list.add("\\BEGIN")

list.add("\":?_/,.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

val z =
(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toString()

list.add(z)

list

list.contains("a")

val lex = List[Char]('a', 'b', 'c')


val lexeme2 = "this is a string"

lexeme2.toCharArray
lexeme2.length

val rand : Char = '\\'

def changeExtension(name : String): String = {
  //output file
  val output = name take(name.length - 4)
  output + ".html"
}

changeExtension("input.mkd")

var filename: String =""
filename = "input.mkd"
filename = changeExtension(filename)