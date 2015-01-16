package domain

case class Case(objects: Map[String, Map[String,Any]] = Map(), id: Long = System.currentTimeMillis())

case class SessionState(objects: Map[String, Map[String,Any]], cases: List[Case], currentCaseIdx: Int)
