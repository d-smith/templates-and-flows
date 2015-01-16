package domain



class ProtoObject private (objStructure: NamedObject) {
  val (name, fieldMap) = objStructure

  override def toString = s"name: $name \n $fieldMap"

}

object ProtoObject {
  def apply(namedObject: NamedObject) : ProtoObject =
    new ProtoObject(namedObject)
}
