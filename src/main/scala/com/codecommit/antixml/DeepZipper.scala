package com.codecommit.antixml

import DeepZipper._
import scala.collection.generic.CanBuildFrom
import com.codecommit.antixml.util.VectorCase
import scala.collection.immutable.{SortedMap, IndexedSeq, Seq}
import scala.collection.IndexedSeqLike
import scala.collection.GenTraversableOnce
import scala.collection.mutable.Builder

/** A zipper which allows deep selection.
 *
 *  Zipper instances may be created through factory methods on the companion.
 */
trait DeepZipper[+A <: Node] extends Group[A] with IndexedSeqLike[A, DeepZipper[A]] { self =>

  /*
   * All the vectors beneath should have the same length.
   */
  
  /** Keeping track of internal time.
   *
   *  Should be initialized to 0 at the initial creation of the zipper.
   */
  protected def time: Time

  protected def parent: Option[DeepZipper[Node]]
  
  private def parentOrError = parent getOrElse sys.error("Root has no parent")

  protected def metas: IndexedSeq[(SimplePath, Time)]

  protected def pathIndex: SortedMap[SimplePath,(Time,IndexedSeq[Int])]

  override protected[this] def newBuilder = DeepZipper.newBuilder[A]
  
  def peek() {
    //TODO
    println("parent="+parent)
    println("time="+time)
    println("metas="+metas)
    println("pathIndex="+pathIndex)
  }

  override def updated[B >: A <: Node](index: Int, node: B): DeepZipper[B] = {
    val updatedTime = time + 1
    val (updatedPath,_) = metas(index)
    val (_,updatePathIndices) = pathIndex(updatedPath)
    
    new Group(super.updated(index, node).toVectorCase) with DeepZipper[B] {
      def parent = self.parent
      val time = updatedTime      
      val metas = self.metas.updated(index, (updatedPath, updatedTime))
      val pathIndex = self.pathIndex.updated(updatedPath,(updatedTime,updatePathIndices))
    }
  }

  override def slice(from: Int, until: Int): DeepZipper[A] = {
    val zwi = Map[A, Int](zipWithIndex: _*)
    collect {
      case e if zwi(e) >= from && zwi(e) < until => e
    }
  }
  
  override def drop(n: Int) = slice(n, size)
  
  override def take(n: Int) = slice(0, n)
  
  override def splitAt(n: Int) = (take(n), drop(n))
  
  override def filter(f: A => Boolean): DeepZipper[A] = collect {
    case e if f(e) => e
  }
  
  override def collect[B, That](pf: PartialFunction[A, B])(implicit cbf: CanBuildFrom[DeepZipper[A], B, That]): That =
    flatMap(pf.lift andThen { _.toTraversable })
    
  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[DeepZipper[A], B, That]): That = {
    val liftedF = (a: A) => Seq(f(a))
    flatMap(liftedF)(cbf)
  }

  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit cbf: CanBuildFrom[DeepZipper[A], B, That]): That = {
    cbf match {
      // subtypes of this are the only expected types, hence ignoring type erasure
      case cbf: CanProduceDeepZipper[DeepZipper[A], B, That] => {
        val liftedF = (x: (A, Int)) => f(x._1)
        flatMapWithIndex(liftedF)(cbf.lift)
      }
      
      case _ => super.flatMap(f)(cbf)
    }
  }

  override def toZipper = this
  
  def stripZipper = new Group(toVectorCase)
  
  /** A specialized flatMap where the mapping function receives the index of the 
   * current element as an argument. */
  private def flatMapWithIndex[B, That](f: ((A, Int)) => GenTraversableOnce[B])(implicit cbfwdz: CanBuildFromWithDeepZipper[DeepZipper[A], B, That]): That = {
    val result = toVector.zipWithIndex map {x => (f(x),x._2)}
    
    val builder = cbfwdz(parent, this)
    for ( (items, index) <- result) {
      val (path, _) = metas(index)
      builder += ((path, time+index+1, items))
    }
    //TODO - Optimize
    for ( (path,(time,inds)) <- pathIndex) {
      if (inds.isEmpty)
        builder += ((path,time,VectorCase.empty[B]))
    }
    builder.result
  }
  
  /** Applying the node updates. */
  lazy val unselect: DeepZipper[Node] = {
    //TODO - Preserve update times?
    parentOrError flatMapWithIndex {
      case (node,index) => pullBack(node, SimplePath(VectorCase(index)))._2
    }
  }
  
  private def pullBacksFor(p: SimplePath): (Time, IndexedSeq[(A,Time)]) = {
    val (time, indices) = pathIndex(p)
    (time, indices map {x => (self(x), metas(x)._2)})
  }
  
  private[antixml] def containsPath(p: SimplePath) = pathIndex.contains(p)
  
  private[antixml] def isBeneathPath(p: SimplePath) = {
    val ifp = pathIndex.keySet.from(p)
    val result = for {
      h <- ifp.headOption
      h2 <- if (h != p) Some(h) else ifp.take(2).tail.headOption
    } yield h2.startsWith(p) && h2.length != p.length
    result.getOrElse(false)    
  }
  
  /* TODO
  private def pullBack(node: Node, path: SimplePath): (Time, IndexedSeq[Node]) = {
    println("pb "+path + " "+pathIndex.keySet.from(path).headOption)
    val r = pullBackx(node,path)
    println("pbr "+path +" -> "+r._1)
    r
  }
  */
  
  private def pullBack(node: Node, path: SimplePath): (Time, IndexedSeq[Node]) = node match {
    case elem: Elem if isBeneathPath(path) => {
      val childPullBacks @ (childTime,childGroup) = pullBackSeq(elem.children, path)
      if (containsPath(path)) {
        mergeConflicts(node, pullBacksFor(path), childPullBacks)
      } else {
        (childTime,VectorCase(elem.copy(children=childGroup)))
      }
    }
    case _ if path liesIn this => {
      val (time,items) = pullBacksFor(path)
      (time, items.map(_._1))
    }
    case _ => (0,VectorCase(node))
  }

  private def pullBackSeq(nodes: IndexedSeq[Node], parentPath: SimplePath): (Time, Group[Node]) = {
    val pbs = nodes.zipWithIndex.map {
      case (node, index) => pullBack(node, parentPath :+ index)
    }
    val maxTime = pbs.maxBy(_._1)._1
    (maxTime, pbs.flatMap[Node,Group[Node]](_._2))
  }
  
  private def mergeConflicts(node: Node, directPullBacks: (Time,IndexedSeq[(Node,Time)]) , childPullBacks: (Time,Group[Node])): (Time, IndexedSeq[Node]) = {
    val (chTime,chGroup) = childPullBacks
    val result = directPullBacks._2 map {
      case (e:Elem,t) if (t<chTime || e.children == node.children) => e.copy(children=chGroup)
      case (n, _) => n
    }
    val rtime = math.max(directPullBacks._1,chTime)
    (rtime, result)
  }
  
}

/** A factory for [[DeepZipper]] instances.
 *  Zippers may be created directly from groups through [[DeepZipper.groupToZipper]] or
 *  through selection using a [[PathFunction]] with [[DeepZipper.fromPath]]/[[DeepZipper.fromPathFunc]]
 *
 *  By importing the implicits in this object any [[Selectable]] can be pimped with
 *  shallow/deep selection methods, which directly take selectors as input.
 *  TODO examples
 */
object DeepZipper {

  import CanBuildFromWithDeepZipper.ElemWithContext
  
  /** The units in which time is measured in the zipper. Assumed non negative. */
  private[antixml] type Time = Int
      
  implicit def canBuildFromWithDeepZipper[A <: Node] = {
    new CanBuildFromWithDeepZipper[Traversable[_], A, DeepZipper[A]] {      
      override def apply(parent: Option[DeepZipper[Node]]): Builder[ElemWithContext[A],DeepZipper[A]] = new WithDeepZipperBuilder[A](parent)
    }
  }
  
  implicit def canBuildFromDeep[A <: Node]: CanBuildFrom[Group[_], A, DeepZipper[A]] = {
    new CanBuildFrom[Group[_], A, DeepZipper[A]] with CanProduceDeepZipper[Group[_], A, DeepZipper[A]] {
      def apply(from: Group[_]): Builder[A, DeepZipper[A]] = apply()
      def apply(): Builder[A, DeepZipper[A]] = DeepZipper.newBuilder[A]

      def lift = canBuildFromWithDeepZipper
    }
  }
  
  def newBuilder[A <: Node] = VectorCase.newBuilder[A].mapResult({new Group(_).toZipper})
  
  private[antixml] def brokenZipper[A <: Node](nodes: VectorCase[A]): DeepZipper[A] = {
    val fakePath = SimplePath(VectorCase(0))
    new Group[A](nodes) with DeepZipper[A] {
      override def parent = None      
      override val time = 0
      override val metas = constant((fakePath,0), nodes.length)
      override val pathIndex: SortedMap[SimplePath,(Time,IndexedSeq[Int])] = SortedMap( fakePath -> ((0, 0 until nodes.length)))
    }
  }
  
  private def constant[A](a: A, sz: Int) = new IndexedSeq[A] {
    override def apply(i: Int) = a
    override def length = sz
  }
  
  private class WithDeepZipperBuilder[A <: Node](parent: Option[DeepZipper[Node]]) extends Builder[ElemWithContext[A],DeepZipper[A]] { self =>
    private val innerBuilder = VectorCase.newBuilder[(SimplePath, Time, A)]
    private var pathIndex = SortedMap.empty[SimplePath,(Time,IndexedSeq[Int])]
    private var size = 0
    private var maxTime = 0
    
    override def += (ewc: ElemWithContext[A]) = {      
      val (path, time, ns) = ewc
      val items = ns.seq.toSeq.map(x => (path, time, x))(VectorCase.canBuildFrom)
      //println("+= "+items) //TODO
      innerBuilder ++= items
      
      val (oldTime,oldInds) = pathIndex.getOrElse(path, (0,VectorCase.empty))
      val (newTime,newInds) = (math.max(oldTime, time), oldInds ++ (size until (size + items.length)))
      pathIndex = pathIndex.updated(path, (newTime,newInds))
      
      size += items.length
      maxTime = math.max(maxTime, time)
      this            
    }
    override def clear() {
      innerBuilder.clear()
      pathIndex = SortedMap.empty[SimplePath,(Time,IndexedSeq[Int])]
      size = 0
      maxTime = 0
    }
    override def result(): DeepZipper[A] = {
      val res = innerBuilder.result()
      new Group[A](res map {case (_,_,node) => node}) with DeepZipper[A] {
        override def parent = self.parent      
        override val time = maxTime
        override val metas = res map {case (path,time,_) => (path,time)}
        override val pathIndex = self.pathIndex
      }
    }
  }
  
}