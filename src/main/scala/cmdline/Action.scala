package cmdline

import domain.{Case, SessionState, ProtoObject}

sealed trait Action
case object Quit extends Action
case object CreateCase extends Action
case object ListCases extends Action
case object ListObjects extends Action
case object SelectCase extends Action
case object ShowCurrentCase extends Action

case object CreateObject extends Action {
  import CmdLineUtils.readLineGroup

  val parser = new dsl.ObjectParser


  def getObject() : Option[ProtoObject] = {
    Console.println("Enter an object (terminate entry with empty line):")
    val rawObj = readLineGroup()
    val parseResult = parser.parseAll(parser.namedObj, rawObj)
    parseResult.successful match {
      case true => Some(ProtoObject(parseResult.get))
      case false =>
        println(s"\n${parseResult.toString}")
        None
    }

  }
}

object ActionHandlers {
  type StateTransitionFunction = PartialFunction[(SessionState, Action),SessionState]

  val QuitActionHandler: StateTransitionFunction = {
    case (gameState, Quit) =>
      System.exit(0)
      gameState //For the compiler
  }

  val CreateCaseActionHandler: StateTransitionFunction = {
    case (gameState@SessionState(_,cases,caseIdx), CreateCase) =>
      println("Enter case name: ")
      val caseName = Console.readLine
      val updatedCases = cases :+ new Case(caseName)
      gameState.copy(cases = updatedCases, currentCaseIdx = updatedCases.length - 1)
  }

  val SelectCaseActionHandler: StateTransitionFunction = {
    case (gameState@SessionState(_,cases,caseIdx), SelectCase) =>
      Console.println("Enter case name: ")
      val caseName = Console.readLine()
      cases.zipWithIndex.filter(_._1.caseName == caseName) match {
        case h :: Nil => gameState.copy(currentCaseIdx = h._2)
        case _ =>
          Console.println(s"Single case named $caseName not found")
          gameState
      }
  }

  val ShowCurrentCaseActionHandler: StateTransitionFunction = {
    case (gameState@SessionState(_,cases, currentCaseIdx), ShowCurrentCase) =>
      cases match {
        case Nil => Console.println("No cases yet")
        case _ =>  Console.println(cases(currentCaseIdx))
      }

      gameState
  }

  var DisplayCasesActionHandler: StateTransitionFunction = {
    case (gameState@SessionState(_, cases,caseIdx), ListCases) =>
      println(s"current case idx is $caseIdx")
      println(s"cases: $cases")
      gameState
  }

  var ListObjectsActionHandler: StateTransitionFunction = {
    case (gameState@SessionState(objs,_,_), ListObjects) =>
      println(objs.foreach(println))
      gameState
  }

  var CreateObjectActionHandler: StateTransitionFunction = {
    case (gameState@SessionState(objCollection,_,_),CreateObject) =>
      CreateObject.getObject() match {
        case Some(r) =>
          gameState.copy(objects = r :: objCollection)
        case None => gameState
      }
  }

  val handlers = (QuitActionHandler :: CreateCaseActionHandler :: ShowCurrentCaseActionHandler :: SelectCaseActionHandler ::
                    ListObjectsActionHandler :: DisplayCasesActionHandler :: CreateObjectActionHandler ::Nil)

  val allHandlers = handlers.reduceLeft(_ orElse _)
}