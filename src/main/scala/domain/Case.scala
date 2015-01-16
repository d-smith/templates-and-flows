package domain

case class Case(caseName: String, objects: List[ProtoObject] = List(), id: Long = System.currentTimeMillis())

case class SessionState(objects: List[ProtoObject], cases: List[Case], currentCaseIdx: Int)

