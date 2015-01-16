package domain

case class Case(objects: List[ProtoObject] = List(), id: Long = System.currentTimeMillis())

case class SessionState(objects: List[ProtoObject], cases: List[Case], currentCaseIdx: Int)
