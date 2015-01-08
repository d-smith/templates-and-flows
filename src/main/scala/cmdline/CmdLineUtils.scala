package cmdline

object CmdLineUtils {
  implicit class StringHelpers(val s: String) {
    import scala.util.control.Exception._
    def toIntOpt = catching(classOf[NumberFormatException]) opt s.toInt
  }
}
