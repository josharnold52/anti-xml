package com.codecommit.antixml

//import DeepZipper._

import util.VectorCase


 /** Defines type related to paths on a tree.
  * Also contains factory methods for [[PathFunction]]s  */
private[antixml] object PathCreator {

  /** The number represents the number of the node in its parent's children list.
   *  In case the node is root, the number is its position in the group to which it belongs.
   */
  private[antixml] type Location = Int
  
  private[antixml] case class PathVal[+A](value: A, path: SimplePath)
  
  private type ReversedPath = List[Location]
    
  /** The values from a path function in raw form. */
  type PathVals[+A] = Seq[(PathVal[A])]
  
  /** A function that creates paths on group, to be used when constructing zippers. */
  type PathFunction[+A] = Group[Node] => PathVals[A]
  
  def reverse(rev: ReversedPath): SimplePath = SimplePath(VectorCase(rev.reverse : _*))
  
  /** A path function that selects on nodes in the given group. */
  def fromNodes[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectGroup(nodes, selector, Nil)
  }

  /** A path function that selects on the given nodes and recursively on the children (breadth first). */
  def all[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectGroupRecursive(List((nodes, Nil)), selector)
  }
  
  /** A path function that selects on the children of the given group. */
  def directChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectChildrenOfGroup(nodes, selector)
  }
  
  /** A path function that selects on the recursively on all the children of the given group (breadth first). */
  def allChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectGroupRecursive(collectGroupChildren(nodes, Nil, selector), selector)
  }
  
  /** Collects items from the given group that match the selector. */
  private def collectGroup[A](nodes: Group[Node], s: Selector[A], p: ReversedPath): PathVals[A] = {
    dispatchSelector(s, nodes) {
      val ni = nodes.zipWithIndex
      for ((n, i) <- ni if s isDefinedAt n) yield PathVal(s(n),reverse(i :: p))
    }
  }
  
  /** Collects items from the list groups that match the selector. */
  private def collectGroups[A](groups: Seq[(Group[Node], ReversedPath)], s: Selector[A]): PathVals[A] = {
    groups flatMap {gp =>
      val (g, p) = gp
      collectGroup(g, s, p)
    }
  }
  
  /** Applies the group selector collection function on the children of the given group. */
  private def collectChildrenOfGroupWith[A]
	  (nodes: Group[Node], s: Selector[A], p: ReversedPath)
	  (toVals: (Group[Node], Selector[A], ReversedPath) => PathVals[A]): PathVals[A] = {
    dispatchSelector(s, nodes) {
      val ni = nodes.zipWithIndex
      ni flatMap {
        case (e: Elem, i) => toVals(e.children, s, i :: p)
        case _ => Nil
      }
    }
  }
  
  /** Collects items from the children of the given group that match the selector. */
  private def collectChildrenOfGroup[A](nodes: Group[Node], s: Selector[A]): PathVals[A] = {
    collectChildrenOfGroupWith(nodes, s, Nil) (collectGroup _)
  }
  
  /** Recursively collects items from the given group that match the selector. */
  private def collectGroupRecursive[A](groups: Seq[(Group[Node], ReversedPath)], s: Selector[A]): PathVals[A] = {
    /*
    TODO
    println("cgr {")
    for(x <- groups.zipWithIndex) println("  "+x._2+" "+x._1)
    println("}")
    */
    if (groups.isEmpty) Nil
    else {
      val allChildren =
        groups flatMap { gp =>
          val (g, p) = gp
          collectGroupChildren(g, p, s)
        }
      collectGroups(groups, s) ++ collectGroupRecursive(allChildren, s)
    }
  }
  
  /** Gathering all the children of the group that may match the selector. */
  private def collectGroupChildren(g: Group[Node], p: ReversedPath, s: Selector[_]): Seq[(Group[Node], ReversedPath)] = {
    dispatchSelector[Seq[(Group[Node], ReversedPath)]](s, g)(Nil) {
      val gi = g.zipWithIndex
      gi flatMap {
        case (e: Elem, i) => Some((e.children, i :: p))
        case _ => None
      }
    }
  }
  
  /** If dispatching on the selector yields true, executing the given code block, otherwise returning 
   * the default value.*/
  private def dispatchSelector[A](s: Selector[_], g: Group[Node])(default: A)(vals: => A): A = {
    if (dispatchSelector(s, g)) vals
    else default
  }

  /** If dispatching on the selector yields true, executing the given code block, otherwise returning
   *  an empty list.
   */
  private def dispatchSelector[A](s: Selector[A], g: Group[Node])(vals: => PathVals[A]): PathVals[A] = {
    dispatchSelector[PathVals[A]](s, g)(Nil)(vals)
  }
  
  /** Returns true if there is a chance that applying the given selector on the group
   * would yield some results. */
  private def dispatchSelector(s: Selector[_], g: Group[Node]) = {
    s match {
      case opt: OptimizingSelector[_] => opt.canMatchIn(g)
      case _ => true // no info about the selector, should proceed
    }
  }

}