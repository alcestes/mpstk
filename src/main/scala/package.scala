package object mpstk {
  /** Abstract representation of payload + continuation.
    * 
    * The type parameter denotes the continuation type.
    */
  abstract class BasePayloadCont[A](payload: Type, cont: A) {
    override def toString = s"(${payload}).${cont}"
  }

  /** Abstract representation of a choice. */
  type Choices[PC <: BasePayloadCont[_]] = Map[Label, PC]
}
