package cmdline

object CmdLineUtils {
  implicit class StringHelpers(val s: String) {
    import scala.util.control.Exception._
    def toIntOpt = catching(classOf[NumberFormatException]) opt s.toInt
  }

  def readLineGroup(): String = {
    var sb = new StringBuilder()
    Iterator.continually(Console.readLine).takeWhile(_.nonEmpty).foreach(line => {sb.append(line); sb.append("")})
    sb.toString()
  }
}
