package oo_db.db.nodes

import scalaz.std.option.optionSyntax._

import scala.annotation.tailrec

trait Node {
	
	def pos: Long
	
	def keys: List[Long]
	
	def size: Int
	
	def toList(order: Int): List[Long]
	
	/**
	  * ====================
	  * This method does not handle recursive inserting behavior,
	  * only the insertion of the specified k/v pair, and notifies the caller
	  * of the outcome, which is there responsibility to handle
	  * ====================
	  *
	  * None =>
	  * 	There were no changes to this node based on the insertion
	  * Option(n1, None) =>
	  * 	There was a change to the node, and it needs to be re-written to disk,
	  * 	but it was not split, so no further changes are necessary
	  * Option(n1, Option(m, n2)) =>
	  * 	There was a change to the node, and it needs to be re-written to disk.
	  * 	It was also split, so the addition needs to be added with 'm' as the key,
	  * 	and 'n2.pos' as the value
	  */
	def insert(order: Int, freeListStart: Long, key: Long, value: Long): Option[(Node, Option[(Long, Node)])]
	
	@tailrec
	protected final def afterInsert(key: Long, value: Long, k: List[Long], v: List[Long], pK: List[Long], pV: List[Long]): Option[(List[Long], List[Long])] = ((k, v): @unchecked) match {
		case (Nil, Nil) =>
			(
				(key :: pK).reverse,
				(value :: pV).reverse
			).some
		case (hK :: tK, hV :: tV) =>
			if (key < hK)
				(
					pK.reverse ++ (key :: k),
					pV.reverse ++ (value :: v)
				).some
			else if (key == hK) {
				if (value == hV)
					None
				else
					(
						pK.reverse ++ k,
						pV.reverse ++ (value :: tV)
					).some
			}
			else
				afterInsert(key, value, tK, tV, hK :: pK, hV :: pV)
	}
	
}

object NodeUtils {
	
	@tailrec
	def takeAtMax(max: Int, done: Boolean, stopAt0: Boolean, list: List[Long], prev: List[Long]): (List[Long], List[Long]) =
		if (max > 0)
			if (done)
				takeAtMax(max - 1, done, stopAt0, list.tail, prev)
			else
				list match {
					case 0 :: tL if stopAt0 =>
						takeAtMax(max - 1, true, stopAt0, tL, prev)
					case hL :: tL =>
						takeAtMax(max - 1, done, stopAt0, tL, hL :: prev)
				}
		else
			(prev.reverse, list)
	
	@tailrec
	def takeFirst(c: Int, list: List[Long], prev: List[Long] = Nil): (List[Long], List[Long]) =
		if (c > 0)
			takeFirst(c - 1, list.tail, list.head :: prev)
		else
			(prev.reverse, list)
	
}
