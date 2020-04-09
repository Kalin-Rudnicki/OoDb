package oo_db.db.nodes

import scalaz.std.option.optionSyntax._

import scala.annotation.tailrec

case class InternalNode(pos: Long, keys: List[Long], children: List[Long]) extends Node {
	// println(s"# InternalNode($pos, $keys, $children)")
	
	override def size: Int =
		children.size
	
	override def toList(order: Int): List[Long] =
		if (size < order)
			children.padTo(order, 0L) ++ keys.padTo(order - 1, 0L)
		else
			children ++ keys
	
	override def insert(order: Int, freeListStart: Long, key: Long, value: Long): Option[(Node, Option[(Long, Node)])] = {
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
	
}

object InternalNode {
	
	def apply(pos: Long, order: Int, list: List[Long]): InternalNode = {
		val (children, keyStart) = NodeUtils.takeAtMax(order, false, true, list, Nil)
		val (keys, _) = NodeUtils.takeAtMax(children.length - 1, false, false, keyStart, Nil)
		new InternalNode(pos, keys, children)
	}
	
}
