package cmdline

import scala.annotation.tailrec

import domain.SessionState
import domain.Case

object TemplatesAndFlows extends App {
  import CmdLineUtils._

  def banner =
    """
      ||  ____| |              / ____|
      || |__  | | _____      _| |     ___  _ __
      ||  __| | |/ _ \ \ /\ / / |    / _ \| '_ \
      || |    | | (_) \ V  V /| |___| (_) | | | |
      ||_|    |_|\___/ \_/\_/  \_____\___/|_| |_|
    """.stripMargin

  val actionMenu = List(
    ("Create a case", CreateCase),
    ("Show current case", ShowCurrentCase),
    ("List cases", ListCases),
    ("Select case", SelectCase),
    ("Create an object", CreateObject),
    ("List objects", ListObjects),
    ("Quit", Quit)
  ).zipWithIndex



  def issueMainPrompt() : Unit = {
    println("Choose an action: ")
    actionMenu map {
      case ((s,_), i) => println(s"$i $s")
    }
  }

  def handleCmd(n: Int, ss: SessionState) : SessionState = {
    import ActionHandlers._
    val selection = actionMenu(n)
    val action = selection._1._2
    sessionLoop(allHandlers.apply(ss, action))
  }

  @tailrec
  def sessionLoop(ss: SessionState) : SessionState = {
    import ActionHandlers._
    issueMainPrompt()

    readLine().trim().toIntOpt match {
      case Some(n) if n >= 0 && n <= actionMenu.length => handleCmd(n,ss)
      case None =>
        println("Select one of the numbers given in the prompt, ok?")
        sessionLoop(ss)
    }
  }

  println(banner)

  val sessionState = new SessionState(List(), List(), -1)
  sessionLoop(sessionState)

}
