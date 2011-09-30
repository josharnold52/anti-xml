package com.codecommit.antixml

import scala.collection.immutable.IndexedSeq

/**
 * Describes the parameters to a merge function.  Used as the input to [[ZipperMergeStrategy]].
 * @param original the original Node that was selected when the zipper was produced.
 * @param directUpdates the direct replacements of the node and their corresponding update times.
 * The direct replacements consist of those nodes that explicitly replaced the original node when the zipper was updated.
 * @param lastDirectUpdate the largest update time of any direct update to the node.  If `directUpdates` is empty, this
 * will be the time that the node was removed. 
 * @param indirectUpdate the "indirect" replacement and associated update time.  The indirect replacement is just the
 * original node with its children replaced by their updates.  
 */
case class ZipperMergeContext(original: Node, directUpdates: IndexedSeq[(Node,Int)], lastDirectUpdate: Int, indirectUpdate: (Node,Int))
