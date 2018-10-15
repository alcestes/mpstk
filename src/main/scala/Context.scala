// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk

// ContextImpl: a mapping from channels (with roles) to session types
// FIXME: there's some trickery to make ContextImpl be a Map: is it worth?
protected[mpstk] class ContextImpl[V](impl: Map[Channel, V])
  extends scala.collection.immutable.Map[Channel, V]
  with scala.collection.immutable.MapLike[Channel, V, ContextImpl[V]] {
  override val empty: ContextImpl[V] = Context.empty.asInstanceOf[ContextImpl[V]]
  override def get(key: Channel): Option[V] = impl.get(key)
  override def iterator: Iterator[(Channel, V)] = impl.iterator
  override def +[V1 >: V](kv: (Channel, V1)): ContextImpl[V1] = new ContextImpl[V1](impl + kv)
  override def -(key: Channel): ContextImpl[V] = new ContextImpl[V](impl - key)
}

protected[mpstk]
class Context(impl: Map[Channel, MPST]) extends ContextImpl[MPST](impl) {
  /** Return a context where all types respect the Barendregt convention */
  def barendregt: Context = ops.barendregt(this)

  /** Return a context where all types have unfolded payloads */
  def unfoldPayloads: Context = {
    Context(this.toSeq.map(ct => (ct._1, ct._2.unfoldPayloads)):_*)
  }

  /** Return the raw version of this context */
  def toRaw: raw.Context = {
    raw.Context(this)
  }
}

/** Session typing context. */
object Context {
  import parser.ContextParser

  /** Create a session typing context with the given entries. */
  def apply(elems: (Channel, MPST)*): Context = {
    new Context(Map(elems:_*))
  }

  /** Create a session typing context by parsing the given string */
  def apply(input: String): ContextParser.ParseResult[Context] = {
    ContextParser.parse(input)
  }

  /** Empty typing context. */
  val empty: Context = new Context(Map())
}
