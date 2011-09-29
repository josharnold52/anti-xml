package com.codecommit.antixml

import scala.collection.GenTraversableOnce
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import DeepZipper._

/** A factory for [[DeepZipper]] instances. 
 * @tparam N The type of nodes to be contained in the [[DeepZipper]] (if any).
 */
trait CanBuildFromWithDeepZipper[-From, -Elem, To] {
  import CanBuildFromWithDeepZipper.ElemWithContext
  
    /** Creates a new builder.
     * 
     *  @param parent The parent of the zipper
     *  @param contexts The contexts from which the zipper should be composed.
     *  The contexts will be merged to the builder's input to produce a zipper.
     *  @parent emptiesSet A set of empty locations in the zipper. */
	def apply(parent: Option[DeepZipper[Node]]): Builder[ElemWithContext[Elem], To]
	
    /** Creates a new builder.
     *  @param parent The parent of the zipper
     *  @param from The collection building the zipper
     *  @param contexts The contexts from which the zipper should be composed.
     *  The contexts will be merged to the builder's input to produce a zipper.
     *  @parent emptiesSet A set of empty locations in the zipper. */
  def apply(parent: Option[DeepZipper[Node]], from: From): Builder[ElemWithContext[Elem], To] = this(parent)

}

/** A marker interface for [[CanBuildFrom]] instances that can be lifted into
 * [[CanBuildFromWithDeepZipper]] instances which operate on [[Node]] types. */
trait CanProduceDeepZipper[-From, A <: Node, To] { this: CanBuildFrom[From, A, _ >: To] =>
  def lift: CanBuildFromWithDeepZipper[From, A, To]
}

/** Different implicit implementations of [[CanBuildFromWithDeepZipper]]. */
object CanBuildFromWithDeepZipper {
  
  type ElemWithContext[+Elem] = (SimplePath, Time, GenTraversableOnce[Elem])
  
  /** Implicitly lifts [[CanBuildFrom]] instances into instances of [[CanBuildFromWithDeepZipper]]. */
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]): CanBuildFromWithDeepZipper[From, Elem, To] = {
    new CanBuildFromWithDeepZipper[From, Elem, To] {
      
      /** Creates a builder that just ignores anything [[DeepZipper]] related. */
      override def apply(parent: Option[DeepZipper[Node]], from: From) = liftBuilder(cbf(from))
      
      /** Creates a builder that just ignores anything [[DeepZipper]] related. */
      override def apply(parent: Option[DeepZipper[Node]]) = liftBuilder(cbf())
      
      private def liftBuilder(b: Builder[Elem,To]) = new Builder[ElemWithContext[Elem], To]() {
        override def += (x: ElemWithContext[Elem]) = {
          b ++= x._3.seq
          this
        }
        override def clear() {
          b.clear()
        }
        override def result() = b.result()
      }
    }
  }
}