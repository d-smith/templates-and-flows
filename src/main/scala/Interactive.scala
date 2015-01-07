
object Interactive extends App {



  import dsl.ObjectParser

  def readLineGroup(): String = {
    var sb = new StringBuilder()
    Iterator.continually(Console.readLine).takeWhile(_.nonEmpty).foreach(line => {sb.append(line); sb.append("")})
    sb.toString()
  }

  val op = new ObjectParser
  Console.println("Give me an object: ")
  val parsed = op.parseAll(op.value, readLineGroup())
  println(parsed)

}
