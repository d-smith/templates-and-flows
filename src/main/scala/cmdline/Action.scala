package cmdline

sealed trait Action
case object Quit extends Action
case object CreateCase extends Action
case object ListCases extends Action

object ActionHandlers {
  val QuitActionHandler: PartialFunction[(SessionState, Action),SessionState] = {
    case (gameState, Quit) =>
      System.exit(0)
      gameState //For the compiler
  }

  val CreateCaseActionHandler: PartialFunction[(SessionState, Action), SessionState] = {
    case (gameState@SessionState(cases,caseIdx), CreateCase) =>
      val updatedCases = cases :+ new Case()
      gameState.copy(updatedCases, updatedCases.length - 1)
  }

  var DisplayCasesActionHandler: PartialFunction[(SessionState, Action), SessionState] = {
    case (gameState@SessionState(cases,caseIdx), ListCases) =>
      println(s"current case idx is $caseIdx")
      println(s"cases: $cases")
      gameState
  }

  val handlers = (QuitActionHandler :: CreateCaseActionHandler :: DisplayCasesActionHandler :: Nil)

  val allHandlers = handlers.reduceLeft(_ orElse _)
}