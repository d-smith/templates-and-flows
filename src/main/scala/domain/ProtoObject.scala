package domain



class ProtoObject private (objStructure: NamedObject) {
  val (name, fieldMap) = objStructure

}

object ProtoObject {
  def apply(namedObject: NamedObject) : ProtoObject =
    new ProtoObject(namedObject)
}
