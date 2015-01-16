package cmdline

import domain.{Case, SessionState, ProtoObject}

sealed trait Action
case object Quit extends Action
case object CreateCase extends Action
case object ListCases extends Action
case object ListObjects extends Action

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
  val QuitActionHandler: PartialFunction[(SessionState, Action),SessionState] = {
    case (gameState, Quit) =>
      System.exit(0)
      gameState //For the compiler
  }

  val CreateCaseActionHandler: PartialFunction[(SessionState, Action), SessionState] = {
    case (gameState@SessionState(_,cases,caseIdx), CreateCase) =>
      val updatedCases = cases :+ new Case()
      gameState.copy(cases = updatedCases, currentCaseIdx = updatedCases.length - 1)
  }

  var DisplayCasesActionHandler: PartialFunction[(SessionState, Action), SessionState] = {
    case (gameState@SessionState(_, cases,caseIdx), ListCases) =>
      println(s"current case idx is $caseIdx")
      println(s"cases: $cases")
      gameState
  }

  var ListObjectsActionHandler: PartialFunction[(SessionState, Action), SessionState] = {
    case (gameState@SessionState(objs,_,_), ListObjects) =>
      println(s"Objects: $objs")
      gameState
  }

  var CreateObjectActionHandler: PartialFunction[(SessionState, Action), SessionState] = {
    case (gameState@SessionState(objCollection,_,_),CreateObject) =>
      CreateObject.getObject() match {
        case Some(r) =>
          gameState.copy(objects = objCollection + (r.name -> r.fieldMap))
        case None => gameState
      }
  }

  val handlers = (QuitActionHandler :: CreateCaseActionHandler ::
                    ListObjectsActionHandler :: DisplayCasesActionHandler :: CreateObjectActionHandler ::Nil)

  val allHandlers = handlers.reduceLeft(_ orElse _)
}