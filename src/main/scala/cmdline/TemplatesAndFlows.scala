package cmdline

case class Case(objects: Map[String, Map[String,Any]] = Map(), id: Long = System.currentTimeMillis())

case class SessionState(cases: List[Case], currentCaseIdx: Int)

object TemplatesAndFlows extends App {

  def banner =
    """
      ||  ____| |              / ____|
      || |__  | | _____      _| |     ___  _ __
      ||  __| | |/ _ \ \ /\ / / |    / _ \| '_ \
      || |    | | (_) \ V  V /| |___| (_) | | | |
      ||_|    |_|\___/ \_/\_/  \_____\___/|_| |_|
    """.stripMargin

  def issueMainPrompt() : Unit = {
    println("Do you want to [c]reate a case, [d]isplay current case objects,")
    println("[s]elect a case, [l]ist cases, [a]dd an object, [r]emove an object, or")
    println("[i]nstantiate a workflow, or [quit]?")
  }

  def sessionLoop(ss: SessionState) : SessionState = {
    import ActionHandlers._
    issueMainPrompt()
    readLine() match {
      case "c" => sessionLoop(allHandlers.apply(ss,CreateCase))
      case "l" => sessionLoop(allHandlers.apply(ss, ListCases))
      case "q" => sessionLoop(allHandlers.apply(ss, Quit))
      case _ => sessionLoop(ss)
    }

  }

  println(banner)

  val sessionState = new SessionState(List(), -1)
  sessionLoop(sessionState)

}
