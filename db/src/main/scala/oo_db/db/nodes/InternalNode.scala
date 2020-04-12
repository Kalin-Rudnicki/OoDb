package oo_db.db.nodes

import scalaz.std.option.optionSyntax._

import scala.annotation.tailrec

case class InternalNode(pos: Long, keys: List[Long], children: List[Long]) extends Node[InternalNode] {
	// println(s"# InternalNode($pos, $keys, $children)")
	
	override def size: Int =
		children.size
	
	override def toList(order: Int): List[Long] =
		if (size < order)
			children.padTo(order, 0L) ++ keys.padTo(order - 1, 0L)
		else
			children ++ keys
	
	override def insert(order: Int, freeListStart: Long, key: Long, value: Long): Option[(InternalNode, Option[(Long, InternalNode)])] =
		afterInsert(key, value, keys, children.tail, Nil, children.head :: Nil) match {
			case None =>
				None
			case Some((newKeys, newValues)) =>
				if (newKeys.length < order)
					(
						new InternalNode(pos, newKeys, newValues),
						None
					).some
				else {
					val (keys1, passBackVal :: keys2) = NodeUtils.takeFirst(order / 2, newKeys)
					val (values1, values2) = NodeUtils.takeFirst(order / 2 + 1, newValues)
					(
						new InternalNode(pos, keys1, values1),
						(
							passBackVal,
							new InternalNode(freeListStart, keys2, values2)
						).some
					).some
				}
		}
	
	
	def childPos(key: Long): Long = {
		@tailrec
		def loop(k: List[Long], c: List[Long]): Long = ((k, c): @unchecked) match {
			case (Nil, hC :: Nil) =>
				hC
			case (hK :: tK, hC :: tC) =>
				if (key < hK)
					hC
				else
					loop(tK, tC)
		}
		
		loop(keys, children)
	}
	
	/**
	  * @return (minKey, pos, left, right)
	  */
	def childAndNeighbors(key: Long): (Option[Long], Long, Option[Long], Option[(Long, Long)]) = {
		// @tailrec
		def loop(
					pK: Option[Long],
					k: List[Long],
					pV: Long,
					pV2: Option[Long],
					v: List[Long]
				): (Option[Long], Long, Option[Long], Option[(Long, Long)]) = ((k, v): @unchecked) match {
			case (Nil, Nil) =>
				(
					pK,
					pV,
					pV2,
					None
				)
			case (hK :: tK, hV :: tV) =>
				if (key < hK)
					((tK, tV): @unchecked) match {
						case (Nil, Nil) =>
							(
								pK,
								pV,
								pV2,
								None
							)
						case (nK :: _, nV :: _) =>
							(
								pK,
								pV,
								pV2,
								(nK, nV).some
							)
					}
				else
					loop(hK.some, tK, hV, pV.some, tV)
		}
		
		// TODO : Instead of tracking extra ahead, and doing more cases,
		//      : keep track of history, and when a case is hit, pull from history
		
		loop(
			None,
			keys,
			children.head,
			None,
			children.tail
		)
	}
	
}

object InternalNode {
	
	def apply(pos: Long, order: Int, list: List[Long]): InternalNode = {
		val (children, keyStart) = NodeUtils.takeAtMax(order, false, true, list, Nil)
		val (keys, _) = NodeUtils.takeAtMax(children.length - 1, false, false, keyStart, Nil)
		new InternalNode(pos, keys, children)
	}
	
}
