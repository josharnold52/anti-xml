
package com.codecommit.antixml
import util.VectorCase
import scala.collection.immutable.IndexedSeq

trait ZipperMergeStrategy {
  /** Returns the sequence of Nodes that should replace the original node for the specified merge context. */
  def apply(context: ZipperMergeContext): Seq[Node]
}


object ZipperMergeStrategy {
  
  /**
   * Returns a [[ZipperMergeStrategy]] obtained by uniformly applying the specified function to each
   * `directUpdate` node in the merge context and concatenating the results.  The function takes the merge context,
   * a directUpdate node and its associated update time as arguments and returns a sequence of replacement nodes.
   */
  def uniformlyApply(f: (ZipperMergeContext, Node,Int) => Seq[Node]): ZipperMergeStrategy = new ZipperMergeStrategy() {
    override def apply(context: ZipperMergeContext) = context.directUpdates.flatMap(n => f(context, n._1,n._2))
  }

  object AlwaysPreferChildren extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = VectorCase(context.indirectUpdate._1)
  }
  
  object AlwaysPreferParents extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = context.directUpdates.map(_._1)
  }
  
  object AlwaysLocal extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = context.directUpdates map {
      case (e:Elem, _) => e.copy(children=context.indirectUpdate._1.children)
      case (n, _) => n
    }
  }
  
  object RequireLocal extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = context.directUpdates map {
      case (e:Elem, _) if (e.children==context.original.children) => e.copy(children=context.indirectUpdate._1.children)
      case n => error("A conflict has been detected in the following node that cannot be resolved by the RequireLocal merge strategy:\n" + n)
    }
  }

  /**
   * A strategy that is similar to Local except that it throws an exception if it 
   */
  object RequireConflictFree extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext): Nothing =
      error("A node and one or more of its descendents were contained in the same zipper.\n" +
          "Possible fixes include either using a different merge strategy or using a different selection\n" +
          "operator.\n"+
          context.original)
  }

  
  implicit object PreferLatest extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = {
      import context._
      val (indRep, indTime) = indirectUpdate
      context.directUpdates map {
          case (e:Elem, time) if ((indTime>=time) || (e.children==original.children)) => e.copy(children=context.indirectUpdate._1.children)
          case (n, _) => n
        }
    }
  }
  
}