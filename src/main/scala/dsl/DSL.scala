package dsl

import scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers}

class ObjectParser extends JavaTokenParsers {

  def namedObj: Parser[(String, Map[String, Any])] =
    ident~":"~obj ^^
      { case objName~":"~obj => (objName, obj)}

  def obj: Parser[Map[String, Any]] =
    "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)

  def arr: Parser[List[Any]] =
    "["~> repsep(value, ",") <~"]"

  def member: Parser[(String, Any)] =
    stringLiteral~":"~value ^^
      { case name~":"~value => (name, value) }

  def value: Parser[Any] = (
    namedObj
      | obj
      | arr
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
      | "null"  ^^ (x => null)
      | "true"  ^^ (x => true)
      | "false" ^^ (x => false)
    )

}

object ParseObject extends ObjectParser {
  def parseObj(objString: String) {
    println(parseAll(value,objString))
  }
}