// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT

/** The Multiparty Session Types toolKit. */
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
