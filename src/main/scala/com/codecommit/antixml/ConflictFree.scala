
package com.codecommit
package antixml

import util._

import scala.collection.mutable.{Builder}

object ConflictFree {
  /**
   * CFZContext correlates a (possibly deep) node in a source Group to a range of top-level nodes in a ConflictFreeZipper.
   * 
   * `sourcePath` represents the path to the source node.  The first element is the index in the top
   * level group, the second is the index into that node's child group, etc.
   *
   * `count` is the number of corresponding top-level nodes in the target zipper.
   *
   * We don't store the offset into the target zipper because in practice, we always work with a sequence of CFZContext, 
   * sorted lexicographically by path.  The offset into the target zipper is then the sum of the counts of the
   * preceeding CFZContext.
   */
  private[antixml] case class CFZContext(sourcePath: IndexedSeq[Int], count: Int) 
  
  implicit def canBuildFromGroupWithConflictFreeZipper[A <: Node]: CanBuildFromWithConflictFreeZipper[Group[_], A, ConflictFreeZipper[A]] = {
    new CanBuildFromWithConflictFreeZipper[Group[_], A, ConflictFreeZipper[A]] {
      def apply(outerSource: Group[_], baseContexts: =>List[CFZContext]): Builder[A, ConflictFreeZipper[A]] = {
        VectorCase.newBuilder[A] mapResult { vec =>
          new Group[A](vec) with ConflictFreeZipper[A] {
            override lazy val contexts = baseContexts
            
            override lazy val source = outerSource match {
              case group: Group[Node] => ConflictFree.toConflictFreeZipper(group)
              case _ => error("No zipper context available")
            }
            
            override val hasValidContext = outerSource.isInstanceOf[Group[Node]]
          }
        }
      }
      
      def apply(baseContexts: =>List[CFZContext]): Builder[A, ConflictFreeZipper[A]] = {
        VectorCase.newBuilder[A] mapResult { vec =>
          new Group(vec) with ConflictFreeZipper[A] {
            override lazy val contexts = baseContexts
            override def source = error("No zipper context available")
            override val hasValidContext = false
          }
        }
      }
      
      def append(left: ConflictFreeZipper[A], right: ConflictFreeZipper[A]) = left ++ right
    }
  }
  
  def toConflictFreeZipper[A <: Node](src: Group[A]): ConflictFreeZipper[A] = src match {
    case cfz: ConflictFreeZipper[A] => cfz
    case _ => {
      new Group[A](src.toVectorCase) with ConflictFreeZipper[A] {
        val contexts = List()
        def source = error("Attempted to move up at root of the tree")
        override val hasValidContext = false
      }
    }
  }
  
  implicit def groupAsConflictFreeSelectable[A <: Node](src: Group[A]): ConflictFreeSelectable[A] = new ConflictFreeSelectable[A] {
    override def toGroup = src
  }
  implicit def elemAsConflictFreeSelectable(e: Elem): ConflictFreeSelectable[Elem] = new ConflictFreeSelectable[Elem] {
    override def toGroup = e.toGroup
  }
}


